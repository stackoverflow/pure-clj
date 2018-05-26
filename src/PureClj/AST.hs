module PureClj.AST where

import Prelude.Compat

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity(..), runIdentity)
import Data.Text (Text)

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

data DefType
  = Top
  | TopPvt
  | LetDef
  deriving (Show, Eq, Read)

data Clj
  = CljNumericLiteral (Either Int Double)
  | CljStringLiteral Text
  | CljKeywordLiteral Text
  | CljCharLiteral Char
  | CljBooleanLiteral Bool
  | CljArrayLiteral [Clj]
  | CljObjectLiteral [(KeyType, Clj)]
  | CljUnary UnaryOperator Clj
  | CljBinary BinaryOperator [Clj]
  | CljArrayIndexer Clj Clj
  | CljAccessor KeyType Clj
  | CljFunction (Maybe Text) [Text] Clj
  | CljApp Clj [Clj]
  | CljVar (Maybe Text) Text
  | CljCond [(Clj, Clj)] (Maybe Clj)
  | CljDef DefType Text Clj
  | CljDeclare [Text]
  | CljLet [Clj] [Clj]
  | CljThrow Clj
  | CljObjectUpdate Clj [(KeyType, Clj)]
  | CljRequire Text Text
  | CljNamespace Text (Maybe Text) (Maybe Clj)
  deriving (Show, Eq, Read)

everywhere :: (Clj -> Clj) -> Clj -> Clj
everywhere f = go where
  go :: Clj -> Clj
  go (CljUnary op c) = f (CljUnary op (go c))
  go (CljBinary op cs) = f (CljBinary op (map go cs))
  go (CljArrayLiteral cs) = f (CljArrayLiteral $ map go cs)
  go (CljObjectLiteral kvs) = f (CljObjectLiteral $ map (fmap go) kvs)
  go (CljArrayIndexer c c2) = f (CljArrayIndexer (go c) (go c2))
  go (CljObjectUpdate c kvs) = f (CljObjectUpdate (go c) (map (fmap go) kvs))
  go (CljAccessor kt c) = f (CljAccessor kt (go c))
  go (CljFunction n ps c) = f (CljFunction n ps (go c))
  go (CljApp c cs) = f (CljApp (go c) (map go cs))
  go (CljDef t name c) = f (CljDef t name (go c))
  go (CljLet cs cs2) = f (CljLet (map go cs) (map go cs2))
  go (CljCond kvs el) = f (CljCond (map (fmap go) kvs) (fmap go el))
  go (CljThrow c) = f (CljThrow (go c))
  go other = f other

everywhereTopDown :: (Clj -> Clj) -> Clj -> Clj
everywhereTopDown f = runIdentity . everywhereTopDownM (Identity . f)

everywhereTopDownM :: (Monad m) => (Clj -> m Clj) -> Clj -> m Clj
everywhereTopDownM f = f >=> go where
  f' = f >=> go
  go (CljUnary op c) = CljUnary op <$> f' c
  go (CljBinary op cs) = CljBinary op <$> traverse f' cs
  go (CljArrayLiteral cs) = CljArrayLiteral <$> traverse f' cs
  go (CljObjectLiteral kvs) = CljObjectLiteral <$> traverse (sndM f') kvs
  go (CljArrayIndexer c c2) = CljArrayIndexer <$> f' c <*> f' c2
  go (CljObjectUpdate c kvs) = CljObjectUpdate <$> f' c <*> traverse (sndM f') kvs
  go (CljAccessor kt c) = CljAccessor kt <$> f' c
  go (CljFunction n ps c) = CljFunction n ps <$> f' c
  go (CljApp c cs) = CljApp <$> f' c <*> traverse f' cs
  go (CljDef t name c) = CljDef t name <$> f' c
  go (CljLet cs cs2) = CljLet <$> traverse f' cs <*> traverse f' cs2
  go (CljCond kvs el) = CljCond <$> (traverse (sndM f') kvs) <*> mapM f' el
  go (CljThrow c) = CljThrow <$> f' c
  go other = f other

sndM :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
sndM f (a, b) = (,) a <$> f b
