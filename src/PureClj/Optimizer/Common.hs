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

isReassigned :: Text -> Clj -> Bool
isReassigned var1 = everything (||) check
  where
    check :: Clj -> Bool
    check (CljFunction (Just name) args _) | var1 == name || var1 `elem` args = True
    check (CljFunction Nothing args _) | var1 `elem` args = True
    check (CljDef _ var2 _) | var1 == var2 = True
    check _ = False

isUsed :: Text -> Clj -> Bool
isUsed var1 = everything (||) check
  where
    check :: Clj -> Bool
    check (CljVar Nothing var) | var1 == var = True
    check _ = False

isDict :: (Text, Text) -> Clj -> Bool
isDict (moduleName, dictName) (CljVar (Just y) x) =
  x == dictName && y == moduleName
isDict _ _ = False

isDict' :: [(Text, Text)] -> Clj -> Bool
isDict' xs cljs = any (`isDict` cljs) xs
