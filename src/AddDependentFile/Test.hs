{-# LANGUAGE TemplateHaskell #-}
module AddDependentFile.Test where

import AddDependentFile ( addDependentFile, addDependentFile', getPackageRoot, (</>) )
import Data.ByteString qualified as BS
import Language.Haskell.TH.Syntax (runIO)
import Prelude


$(
  getPackageRoot >>= addDependentFile . (</> "README.md") >> pure []
 )


$(do
     readmeAbsPath <- (</> "README.md") <$> getPackageRoot
     !readmeContent <- runIO $ BS.readFile readmeAbsPath
     addDependentFile' readmeAbsPath readmeContent
     pure []
 )
