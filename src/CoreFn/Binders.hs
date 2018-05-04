module CoreFn.Binders where

import CoreFn.Literal (Literal)
import CoreFn.Ident (Ident)
import CoreFn.Names (Qualified, ProperName)

data Binder a
  = NullBinder a
  | LiteralBinder a (Literal (Binder a))
  | VarBinder a Ident
  | ConstructorBinder a (Qualified ProperName) (Qualified ProperName) [Binder a]
  | NamedBinder a Ident (Binder a)
  deriving (Eq, Ord, Functor, Show)
