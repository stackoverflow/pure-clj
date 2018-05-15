module CoreFn.Binders where

import Prelude.Compat

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

extractBinderAnn :: Binder a -> a
extractBinderAnn (NullBinder a) = a
extractBinderAnn (LiteralBinder a _) = a
extractBinderAnn (VarBinder a _) = a
extractBinderAnn (ConstructorBinder a _ _ _) = a
extractBinderAnn (NamedBinder a _ _) = a
