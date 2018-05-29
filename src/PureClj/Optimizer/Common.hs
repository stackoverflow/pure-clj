module PureClj.Optimizer.Common where

import Prelude.Compat

import Data.List (foldl')
import Data.Text (Text)

import PureClj.AST

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id

replaceIdent :: Text -> Clj -> Clj -> Clj
replaceIdent var1 clj = everywhere replace
  where
  replace (CljVar _ var2) | var1 == var2 = clj
  replace other = other

isDict :: (Text, Text) -> Clj -> Bool
isDict (moduleName, dictName) (CljVar (Just y) x) =
  x == dictName && y == moduleName
isDict _ _ = False

isDict' :: [(Text, Text)] -> Clj -> Bool
isDict' xs cljs = any (`isDict` cljs) xs
