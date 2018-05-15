module Main
  ( main
  ) where

import Prelude.Compat

import CoreFn
import CoreFn.FromJSON
import PureClj.AST
import PureClj.CodeGen
import PureClj.Printer (prettyPrintClj)

import Data.Aeson
import Data.Aeson.Types
import Data.Version
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.IO as TIO
import Control.Monad.Supply
import Control.Monad.Supply.Class

main :: IO ()
main = do
  --contents <- readFile "test/resources/corefn_simple.json"
  contents <- readFile "/home/islon/dev/repos/puretest/output/Test/corefn.json"
  let parsed = decode $ toLazyStr contents :: Maybe Value
  case parsed of
    Just js -> do
      let res = parseModule js
      let res' = evalSupply 0 $ toClj res
      putStrLn ""
      TIO.putStrLn $ prettyPrintClj res'
    Nothing -> print "nope"
  where
    toLazyStr = BL.fromStrict . B.pack

    toClj :: (Monad m, MonadSupply m) => Result (a, Module Ann) -> m [Clj]
    toClj (Success (_, m)) = moduleToClj m
    toClj (Error s) = error $ "Failed while trying to decode json " ++ s

parseModule :: Value -> Result (Version, Module Ann)
parseModule = parse moduleFromJSON
