module PureClj.Build
  ( compileCoreFns
  ) where

import Prelude.Compat

import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE

import Control.Monad.Supply
import Control.Monad.Supply.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Text (Text)
import Data.Version

import CoreFn
import CoreFn.FromJSON
import PureClj.AST
import PureClj.CodeGen
import PureClj.Printer (prettyPrintClj)

compileCoreFns :: [(FilePath, Text)] -> [(FilePath, Text)]
compileCoreFns entries = do
  map compileCoreFn entries

compileCoreFn :: (FilePath, Text) -> (FilePath, Text)
compileCoreFn entry@(path, _) =
  let clj = evalSupply 0 $ jsonToClj entry
  in (path, prettyPrintClj clj)

decodeT :: FromJSON a => Text -> Maybe a
decodeT = decode . TLE.encodeUtf8 . TL.fromStrict

parseModule :: Value -> Result (Version, Module Ann)
parseModule = parse moduleFromJSON

jsonToClj :: (Monad m, MonadSupply m) => (FilePath, Text) -> m [Clj]
jsonToClj (path, content) =
  let decoded = decodeT content :: Maybe Value
      module' = maybe (error $ "Could not decode CoreFn file: " ++ path) parseModule decoded
  in case module' of
    Success (_, m) -> do
      clj <- moduleToClj m
      return clj
      --return (runModuleNameList (moduleName m), clj)
    Error e -> error $ "Failed to parse CoreFn: " ++ e
