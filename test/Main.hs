module Main
  ( main
  ) where

import CoreFn
import CoreFn.FromJSON
import PureClj.AST
import PureClj.CodeGen

import Data.Aeson
import Data.Aeson.Types
import Data.Version
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  contents <- readFile "test/resources/corefn_simple.json"
  let parsed = decode $ toLazyStr contents :: Maybe Value
  case parsed of
    Just js -> print $ toClj $ parseModule js
    Nothing -> print "nope"
  where
    toLazyStr = BL.fromStrict . B.pack

    toClj (Success (v, m)) = moduleToClj m
    toClj (Error s) = [CljVar s]

parseModule :: Value -> Result (Version, Module Ann)
parseModule = parse moduleFromJSON
