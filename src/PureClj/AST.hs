module PureClj.AST where

import Data.Text (Text)
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
  | ShiftLeft
  | ShiftRight
  deriving (Show, Eq, Read)

data KeyType
  = KeyStr Text
  | KeyWord Text
  deriving (Show, Eq, Read)

data Clj
  = CljNumericLiteral (Either Int Double)
  | CljStringLiteral Text
  | CljKeywordLiteral Text
  | CljCharLiteral Char
  | CljBooleanLiteral Bool
  | CljArrayLiteral [Clj]
  | CljObjectLiteral [(KeyType, Clj)]
  | CljUnaryOperator UnaryOperator Clj
  | CljBinaryOperator BinaryOperator Clj Clj
  | CljArrayIndexer Clj Clj
  | CljAccessor KeyType Clj
  | CljFunction (Maybe Text) [Text] Clj
  | CljApp Clj [Clj]
  | CljVar (Maybe Text) Text
  | CljIfElse Clj Clj (Maybe Clj)
  | CljDef Bool Text (Maybe Clj)
  | CljLet [Clj] Clj
  -- | CljThrow Clj
  -- | CljInstanceOf Clj Clj
  -- | CljComment [Comment] Clj
  | CljObjectUpdate Clj [(KeyType, Clj)]
  | CljRequire Text Text
  deriving (Show, Eq, Read)
