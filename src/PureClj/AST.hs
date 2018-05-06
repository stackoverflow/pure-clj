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

data Clj
  = CljNumericLiteral (Either Int Double)
  | CljStringLiteral String
  | CljCharLiteral Char
  | CljBooleanLiteral Bool
  | CljArrayLiteral [Clj]
  | CljObjectLiteral [(String, Clj)]
  | CljUnaryOperator UnaryOperator Clj
  | CljBinaryOperator BinaryOperator Clj Clj
  | CljArrayIndexer Clj Clj
  | CljAccessor String Clj
  | CljAccessorKeyword String Clj
  | CljFunction (Maybe String) [String] Clj
  | CljApp Clj [Clj]
  | CljVar String
  | CljIfElse Clj Clj (Maybe Clj)
  | CljVarIntroduction String (Maybe Clj)
  | CljThrow Clj
  | CljInstanceOf Clj Clj
  | CljComment [Comment] Clj
  deriving (Show, Eq, Read)
