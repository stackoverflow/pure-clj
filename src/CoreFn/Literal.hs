module CoreFn.Literal where

import Prelude.Compat

import Data.Text (Text)

data Literal a
  = NumericLiteral (Either Int Double)
  | StringLiteral Text
  | CharLiteral Char
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  | ObjectLiteral [(Text, a)]
  deriving (Eq, Ord, Show, Functor)
