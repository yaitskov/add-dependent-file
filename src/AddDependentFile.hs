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
import Data.List qualified as L
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
import System.Directory (listDirectory, makeAbsolute)
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
stripInplace pn = subRegex massagePackageName pn "\\1"
  where
    massagePackageName =
      mkRegex
      "^([A-Z_a-z0-9-]+)[-]([0-9]+[.])+[0-9]+.*$"

findCabalFile :: Q (Either String FilePath)
findCabalFile = do
  cabalFileNamePrefix <- stripInplace . loc_package <$> location
  let cabalFileP fp =
        cabalFileNamePrefix `L.isPrefixOf` fp && ".cabal" `L.isSuffixOf` fp
  pkgDir <- getPackageRoot
  do
    pkgDirEntries <- filter cabalFileP <$> runIO (listDirectory pkgDir)
    case pkgDirEntries of
      [] ->
        pure . Left $ "No cabal file with prefix [" <> cabalFileNamePrefix
          <> "] in [" <> pkgDir <> "]"
      [cfn] ->
        pure . pure $ pkgDir </> cfn
      _ ->
        pure . Left $ "Multiple cabal files with prefix [" <> cabalFileNamePrefix
          <> "] in [" <> pkgDir <> "]: " <> show pkgDirEntries

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
      findCabalFile >>= \case
        Left e -> reportError e
        Right cf ->
          assertCabalExtraSourceContains pRelativeToCabal cf
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
