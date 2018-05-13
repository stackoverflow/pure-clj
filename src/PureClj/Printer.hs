module PureClj.Printer
  ( prettyPrintClj
  ) where

import Control.Arrow ((<+>))
import qualified Control.Arrow as A
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)

import PureClj.AST
import PureClj.Pretty.Common

prettyPrintString :: Text -> Text
prettyPrintString s = "\"" <> prettyPrintSymbol s <> "\""

prettyPrintSymbol :: Text -> Text
prettyPrintSymbol s = foldMap encodeChar (T.unpack s)
  where
  encodeChar :: Char -> Text
  encodeChar c | (fromEnum c) > 0xFF = "\\u" <> hex 4 c
  encodeChar c | (fromEnum c) > 0x7E || (fromEnum c) < 0x20 = "\\x" <> hex 2 c
  encodeChar c | c == '\b' = "\\b"
  encodeChar c | c == '\t' = "\\t"
  encodeChar c | c == '\n' = "\\n"
  encodeChar c | c == '\v' = "\\v"
  encodeChar c | c == '\f' = "\\f"
  encodeChar c | c == '\r' = "\\r"
  encodeChar c | c == '"'  = "\\\""
  encodeChar c | c == '\\' = "\\\\"
  encodeChar c = T.singleton $ c

  hex :: (Enum a) => Int -> a -> Text
  hex width c =
    let hs = showHex (fromEnum c) "" in
    T.pack (replicate (width - length hs) '0' <> hs)

literals :: (Emit gen) => Pattern PrinterState Clj gen
literals = mkPattern' match
  where
    match :: (Emit gen) => Clj -> StateT PrinterState Maybe gen
    match (CljNumericLiteral n) = return $ emit $ T.pack $ either show show n
    match (CljStringLiteral s) = return $ emit $ prettyPrintString s
    match (CljKeywordLiteral k) = return $ emit $ ":" <> prettyPrintSymbol k
    match (CljCharLiteral c) = return $ emit $ "\\" <> prettyPrintSymbol (T.singleton c)
    match (CljBooleanLiteral True) = return $ emit "true"
    match (CljBooleanLiteral False) = return $ emit "false"
    match (CljArrayLiteral arr) = mconcat <$> sequence
      [ return $ emit "["
      , intercalate (emit " ") <$> forM arr prettyPrintClj'
      , return $ emit "]"
      ]
    match (CljObjectLiteral []) = return $ emit "{}"
    match (CljObjectLiteral ps) = mconcat <$> sequence
      [ return $ emit "{"
      , withIndent $ do
          entries <- forM ps $ \(key, value) -> fmap ((keyToStr key <> emit " ") <>) . prettyPrintClj' $ value
          indentString <- currentIndent
          return $ intercalate (emit ",\n") $ map (indentString <>) entries
      , return $ emit "}"
      ]
    match (CljVar (Just mn) name) = return $ emit mn <> emit "/" <> emit name
    match (CljVar Nothing name) = return $ emit name
    match (CljDef True ident val) = mconcat <$> sequence
      [ return $ emit $ "(def " <> ident
      , maybe (return mempty) (fmap (emit " " <>) . prettyPrintClj') val
      , return $ emit ")"
      ]
    match (CljDef False ident (Just val)) = mconcat <$> sequence
      [ return $ emit (ident <> " ")
      , prettyPrintClj' val
      ]
    match (CljLet [] ret) = prettyPrintClj' ret
    match (CljLet binds ret) = mconcat <$> sequence
      [ return $ emit "(let [\n"
      , withIndent $ do
          entries <- forM binds $ \bind -> prettyPrintClj' bind
          identString <- currentIndent
          return $ intercalate (emit "\n") $ map (identString <>) entries
      , return $ emit "]\n"
      , prettyPrintClj' ret
      , return $ emit ")"
      ]
    match (CljIfElse cond th el) = mconcat <$> sequence
      [ return $ emit "(if "
      , prettyPrintClj' cond
      , printThenElse th
      , maybe (return mempty) printThenElse el
      , return $ emit ")"
      ]
      where
        printThenElse :: (Emit gen) => Clj -> StateT PrinterState Maybe gen
        printThenElse els = withIndent $ do
          el' <- prettyPrintClj' els
          identString <- currentIndent
          return $ identString <> el'
    match (CljObjectUpdate m keyvals) = mconcat <$> sequence
      [ return $ emit "(assoc "
      , prettyPrintClj' m
      , return $ emit "\n"
      , withIndent $ do
          entries <- forM keyvals $ \(key, value) -> fmap ((keyToStr key <> emit " ") <>) . prettyPrintClj' $ value
          indentString <- currentIndent
          return $ intercalate (emit ",\n") $ map (indentString <>) entries
      , return $ emit ")"
      ]
    match (CljRequire m as) = return $ emit ("[" <> m <> " :as " <> as <> "]")
    match _ = mzero

accessor :: Pattern PrinterState Clj (Text, Clj)
accessor = mkPattern match
  where
  match (CljAccessor (KeyWord prop) val) = Just (T.cons ':' prop, val)
  match (CljAccessor (KeyStr prop) val) = Just (prop, val)
  match _ = Nothing

indexer :: (Emit gen) => Pattern PrinterState Clj (gen, Clj)
indexer = mkPattern' match
  where
  match (CljArrayIndexer index val) = (,) <$> prettyPrintClj' index <*> pure val
  match _ = mzero

app :: (Emit gen) => Pattern PrinterState Clj (gen, Clj)
app = mkPattern' match
  where
  match (CljApp val args) = do
    jss <- traverse prettyPrintClj' args
    return (intercalate (emit " ") jss, val)
  match (CljUnaryOperator Not val) = do
    jss <- traverse prettyPrintClj' [val]
    return (intercalate (emit " ") jss, CljVar Nothing "not")
  match (CljUnaryOperator Negate val) = do
    jss <- traverse prettyPrintClj' [val]
    return (intercalate (emit " ") jss, CljVar Nothing "-")
  match _ = mzero

lam :: Pattern PrinterState Clj ((Maybe Text, [Text]), Clj)
lam = mkPattern match
  where
  match (CljFunction name args ret) = Just ((name, args), ret)
  match _ = Nothing

binary :: (Emit gen) => BinaryOperator -> Text -> Operator PrinterState Clj gen
binary op str = AssocL match (\v1 v2 -> emit "(" <> emit str <> sp <> v1 <> sp <> v2 <> emit ")")
  where
  sp :: (Emit gen) => gen
  sp = emit " "
  match :: Pattern PrinterState Clj (Clj, Clj)
  match = mkPattern match'
    where
    match' (CljBinaryOperator op' v1 v2) | op' == op = Just (v1, v2)
    match' _ = Nothing

prettyStatements :: (Emit gen) => [Clj] -> StateT PrinterState Maybe gen
prettyStatements sts = do
  cljs <- forM sts prettyPrintClj'
  indentString <- currentIndent
  return $ intercalate (emit "\n") $ map (indentString <>) cljs

prettyPrintClj :: [Clj] -> Text
prettyPrintClj = maybe (error "Incomplete pattern") runPlainString . flip evalStateT (PrinterState 0) . prettyStatements

prettyPrintClj' :: (Emit gen) => Clj -> StateT PrinterState Maybe gen
prettyPrintClj' = A.runKleisli $ runPattern matchValue
  where
    matchValue :: (Emit gen) => Pattern PrinterState Clj gen
    matchValue = buildPrettyPrinter operators (literals <+> fmap parensPos matchValue)

    operators :: (Emit gen) => OperatorTable PrinterState Clj gen
    operators =
      OperatorTable [ [ Wrap accessor $ \prop val ->
                          emit "(get " <> val <> sp <> emit prop <> emit ")" ]
                    , [ Wrap indexer $ \index val ->
                          emit "(nth " <> val <> sp <> index <> emit ")" ]
                    , [ Wrap app $ \args val ->
                          emit "(" <> val <> sp <> args <> emit ")" ]
                    , [ Wrap lam $ \(name, args) ret ->
                          emit ("(fn " <> fromMaybe "" name
                                       <> "[" <> intercalate " " args <> "] ")
                               <> ret ]
                    , [ binary Add "+" ]
                    , [ binary Subtract "-" ]
                    , [ binary Multiply "*" ]
                    , [ binary Divide "/" ]
                    , [ binary Modulus "mod" ]
                    , [ binary StringAppend "str" ]
                    , [ binary Equal "=" ]
                    , [ binary NotEqual "not=" ]
                    , [ binary LessThan "<" ]
                    , [ binary LessThanOrEqual "<=" ]
                    , [ binary GreaterThan ">" ]
                    , [ binary GreaterThanOrEqual ">=" ]
                    , [ binary And "and" ]
                    , [ binary Or "or" ]
                    , [ binary BitAnd "bit-and" ]
                    , [ binary BitOr "bit-or" ]
                    , [ binary ShiftLeft "bit-shift-left" ]
                    , [ binary ShiftRight "bit-shift-right" ]]
      where
        sp :: (Emit gen) => gen
        sp = emit " "

keyToStr :: (Emit gen) => KeyType -> gen
keyToStr (KeyStr s) = emit $ prettyPrintString s
keyToStr (KeyWord k) = emit $ prettyPrintString $ ":" <> k