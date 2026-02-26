{-# LANGUAGE OverloadedRecordDot #-}
module AddDependentFile
  ( addDependentFile
  , addDependentFile'
  , getPackageRoot
  , (</>)
  ) where

import Control.Monad ( unless )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.ByteString qualified as BS
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )
import Distribution.Simple.PackageDescription ( readGenericPackageDescription )
import Distribution.Types.PackageDescription ( PackageDescription(extraSrcFiles) )
import Distribution.Utils.Path ( getSymbolicPath, makeSymbolicPath )
import Distribution.Simple.Utils ( isAbsoluteOnAnyPlatform )
import Distribution.Verbosity ( verbose )
import Language.Haskell.TH.Syntax
    ( Q, location, reportError, Loc(loc_package), runIO )
import Language.Haskell.TH.Syntax qualified as TH
import Prelude
import System.Directory (makeAbsolute)
import System.FilePath ( (</>), addTrailingPathSeparator )
import Text.Regex ( mkRegex, subRegex )

-- copy from Distribution.Simple.Utils
stripCommonPrefix :: String -> String -> String
stripCommonPrefix (x : xs) (y : ys)
  | x == y = stripCommonPrefix xs ys
  | otherwise = y : ys
stripCommonPrefix _ ys = ys

parseCabalFile :: MonadIO m => FilePath -> m PackageDescription
parseCabalFile cabPath =
  flattenPackageDescription <$> liftIO (readGenericPackageDescription verbose Nothing (makeSymbolicPath cabPath))

stripInplace :: String -> String
stripInplace pn = subRegex massagePackageName pn "\\1.cabal"
  where
    massagePackageName = mkRegex "^(.+)[-][.0-9]+[-]inplace([.]cabal)*$"

findCabalFile :: Q FilePath
findCabalFile = do
  cabalFileName <- stripInplace . loc_package <$> location
  (</> cabalFileName) <$> getPackageRoot

assertCabalExtraSourceContains :: FilePath -> FilePath -> Q ()
assertCabalExtraSourceContains fp cabalFile = do
  pd <- runIO $ do
    parseCabalFile cabalFile
  let extraSourceFiles :: [FilePath] = getSymbolicPath <$> pd.extraSrcFiles in
    unless (fp `elem` extraSourceFiles) $ do
      reportError $ "File [" <> fp
        <> "] is not mentioned in the extra-source-files section of "
        <> cabalFile <> ". Changes in that file wouldn't trigger rebuild."

-- | Calls 'Language.Haskell.TH.Syntax.addDependentFile' and checks the argument
-- should be an absolute path and the location it refers
-- is mentioned in the @extra-sources-files@ section of the cabal file
addDependentFile :: FilePath -> Q ()
addDependentFile p = do
  if isAbsoluteOnAnyPlatform p
    then do
      pRelativeToCabal <- (`stripCommonPrefix` p) . addTrailingPathSeparator <$> getPackageRoot
      assertCabalExtraSourceContains pRelativeToCabal =<< findCabalFile
      TH.addDependentFile p
    else
    reportError $ "addDependentFile got [" <> p <> "] that is not an absolute path"

-- | Besides 'addDependentFile' checks, it enusures that consumed content is not changed
addDependentFile' :: FilePath -> BS.ByteString -> Q ()
addDependentFile' p expectedContent = do
  addDependentFile p
  pContent <- runIO $ BS.readFile p
  unless (pContent == expectedContent) $
    reportError $ "File [" <> p <> "] has changed after it is consumed"


liftIO1 :: (a -> IO b) -> a -> Q b
liftIO1 f x = runIO (f x)

-- | Absolute path to 'Language.Haskell.TH.Syntax.getPackageRoot'
getPackageRoot :: Q FilePath
getPackageRoot = TH.getPackageRoot >>= liftIO1 makeAbsolute
