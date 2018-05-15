module CoreFn.Meta where

import Prelude.Compat

import CoreFn.Ident (Ident)

data Meta
  = IsConstructor ConstructorType [Ident]
  | IsNewtype
  | IsTypeClassConstructor
  | IsForeign
  | IsWhere
  deriving (Show, Eq, Ord)

data ConstructorType
  = ProductType
  | SumType
  deriving (Show, Eq, Ord)
