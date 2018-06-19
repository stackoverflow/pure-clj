module Clojure.Parser where

import Prelude.Compat

import Text.ParserCombinators.Parsec hiding (spaces)

data CljVal
  = Symbol String
  | List [CljVal]
  | Long Int
  | Double Float
  | String String
  | Boolean Bool
  | Keyword String
  deriving (Read, Show, Eq)
