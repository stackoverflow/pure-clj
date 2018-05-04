module CoreFn.Literal where

import qualified Data.Text as T

data Literal a
  = NumericLiteral (Either Integer Double)
  | StringLiteral T.Text
  | CharLiteral Char
  | BooleanLiteral Bool
  | ArrayLiteral [a]
  | ObjectLiteral [(T.Text, a)]
  deriving (Eq, Ord, Show, Functor)
