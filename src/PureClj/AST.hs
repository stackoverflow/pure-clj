module PureClj.AST where

import CoreFn.Ann (Comment)

data UnaryOperator
  -- | Numeric negation
  = Negate
  -- | Boolean negation
  | Not
  -- | Bit not
  | BitwiseNot
  -- | Numeric plus
  | Positive
  -- | New object
  | New
  deriving (Show, Eq, Read)

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Modulus
  | StringAppend
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | ShiftLeft
  | ShiftRight
  | UnsignedShiftRight
  deriving (Show, Eq, Read)

data KeyType
  = KeyStr String
  | KeyWord String
  deriving (Show, Eq, Read)

data Clj
  = CljNumericLiteral (Either Int Double)
  | CljStringLiteral String
  | CljCharLiteral Char
  | CljBooleanLiteral Bool
  | CljArrayLiteral [Clj]
  | CljObjectLiteral [(KeyType, Clj)]
  | CljUnaryOperator UnaryOperator Clj
  | CljBinaryOperator BinaryOperator Clj Clj
  | CljArrayIndexer Clj Clj
  | CljAccessor KeyType Clj
  | CljFunction (Maybe String) [String] Clj
  | CljApp Clj [Clj]
  | CljVar (Maybe String) String
  | CljIfElse Clj Clj (Maybe Clj)
  | CljDef String (Maybe Clj)
  | CljLet [Clj] Clj
  | CljThrow Clj
  | CljInstanceOf Clj Clj
  | CljComment [Comment] Clj
  | CljObjectUpdate Clj [(KeyType, Clj)]
  deriving (Show, Eq, Read)
