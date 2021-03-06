module PureClj.AST where

import Prelude.Compat

import Control.Arrow ((***))
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
  | BitXor
  | ShiftLeft
  | ShiftRight
  | UnsignedShiftRight
  deriving (Show, Eq, Read)

data KeyType
  = KeyStr Text
  | KeyWord Text
  deriving (Show, Eq, Read)

runKeyType :: KeyType -> Text
runKeyType (KeyStr k) = k
runKeyType (KeyWord k) = k

data DefType
  = Top
  | TopPvt
  | LetDef
  deriving (Show, Eq, Read)

data Clj
  = CljNumericLiteral (Either Int Double)
  | CljStringLiteral (Either Text [Int])
  | CljKeywordLiteral Text
  | CljCharLiteral Char
  | CljBooleanLiteral Bool
  | CljArrayLiteral [Clj]
  | CljObjectLiteral [(KeyType, Clj)]
  | CljUnary UnaryOperator Clj
  | CljBinary BinaryOperator [Clj]
  | CljArrayIndexer Clj Clj
  | CljAccessor KeyType Clj
  | CljFunction (Maybe Text) [Text] [Clj]
  | CljApp Clj [Clj]
  | CljVar (Maybe Text) Text
  | CljCond [(Clj, Clj)] (Maybe Clj)
  | CljIf Clj Clj Clj
  | CljDef DefType Text Clj
  | CljDeclare [Text]
  | CljLet [Clj] [Clj]
  | CljThrow Clj
  | CljObjectUpdate Clj [(KeyType, Clj)]
  | CljRequire Text Text
  | CljNamespace Text (Maybe Text) (Maybe Clj)
  | CljNoOp
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
  go (CljFunction n ps c) = f (CljFunction n ps (map go c))
  go (CljApp c cs) = f (CljApp (go c) (map go cs))
  go (CljDef t name c) = f (CljDef t name (go c))
  go (CljLet cs cs2) = f (CljLet (map go cs) (map go cs2))
  go (CljCond kvs el) = f (CljCond (map (go *** go)  kvs) (fmap go el))
  go (CljIf cond then' else') = f (CljIf (go cond) (go then') (go else'))
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
  go (CljFunction n ps c) = CljFunction n ps <$> traverse f' c
  go (CljApp c cs) = CljApp <$> f' c <*> traverse f' cs
  go (CljDef t name c) = CljDef t name <$> f' c
  go (CljLet cs cs2) = CljLet <$> traverse f' cs <*> traverse f' cs2
  go (CljCond kvs el) = CljCond <$> (traverse (mapTM f') kvs) <*> mapM f' el
  go (CljIf c c2 c3) = CljIf <$> f' c <*> f' c2 <*> f' c3
  go (CljThrow c) = CljThrow <$> f' c
  go other = f other

everything :: (r -> r -> r) -> (Clj -> r) -> Clj -> r
everything (<>) f = go where
  go c@(CljUnary _ c1) = f c <> go c1
  go c@(CljBinary _ cs) = foldl (<>) (f c) (map go cs)
  go c@(CljArrayLiteral cs) = foldl (<>) (f c) (map go cs)
  go c@(CljArrayIndexer c1 c2) = f c <> go c1 <> go c2
  go c@(CljAccessor _ c1) = f c <> go c1
  go c@(CljObjectLiteral cs) = foldl (<>) (f c) (map (go . snd) cs)
  go c@(CljObjectUpdate c1 cs) = foldl (<>) (f c <> go c1) (map (go . snd) cs)
  go c@(CljFunction _ _ c1) = foldl (<>) (f c) (map go c1)
  go c@(CljApp c1 cs) = foldl (<>) (f c <> go c1) (map go cs)
  go c@(CljDef _ _ c1) = f c <> go c1
  go c@(CljLet cs1 cs2) = foldl (<>) (foldl (<>) (f c) (map go cs1)) (map go cs2)
  go c@(CljCond cs1 else') =
    let else'' = case else' of
          Nothing -> []
          Just el -> [el]
        cljs = (map fst cs1) ++ (map snd cs1) ++ else''
    in foldl (<>) (f c) (map go cljs)
  go c@(CljIf c1 c2 c3) = f c <> go c1 <> go c2 <> go c3
  go c@(CljThrow c1) = f c <> go c1
  go other = f other

sndM :: (Functor f) => (b -> f c) -> (a, b) -> f (a, c)
sndM f (a, b) = (,) a <$> f b

mapTM :: (Applicative f) => (a -> f b) -> (a, a) -> f (b, b)
mapTM f (a, b) = (,) <$> f a <*> f b
