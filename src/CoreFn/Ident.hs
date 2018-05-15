module CoreFn.Ident where

import Prelude.Compat

import Data.Text (Text)

data Ident
  = Ident Text
  | GenIdent (Maybe Text) Int
  | UnusedIdent
  deriving (Eq, Ord, Show)
