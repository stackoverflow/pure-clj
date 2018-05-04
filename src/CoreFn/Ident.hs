module CoreFn.Ident where

import qualified Data.Text as T

data Ident
  = Ident T.Text
  | GenIdent (Maybe T.Text) Integer
  | UnusedIdent
  deriving (Eq, Ord, Show)
