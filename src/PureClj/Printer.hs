module PureClj.Printer
  ( prettyPrintClj
  ) where

import Prelude.Compat

import Control.Arrow ((<+>))
import qualified Control.Arrow as A
import Control.Monad (forM, mzero)
import Control.Monad.State (StateT, evalStateT)
import Control.PatternArrows
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
      [ return $ emit "{\n"
      , withIndent $ do
          entries <- forM ps $ \(key, value) -> fmap ((keyToStr key <> emit " ") <>) . prettyPrintClj' $ value
          indentString <- currentIndent
          return $ intercalate (emit ",\n") $ map (indentString <>) (entries <> [emit "}"])
      ]
    match (CljVar (Just mn) name) = return $ emit mn <> emit "/" <> emit name
    match (CljVar Nothing name) = return $ emit name
    match (CljDeclare idents) = return $ emit ("(declare " <> (intercalate " " idents) <> ")")
    match (CljDef LetDef ident val) = mconcat <$> sequence
      [ return $ emit (ident <> " ")
      , prettyPrintClj' val
      ]
    match (CljDef deft ident val) = mconcat <$> sequence
      [ return $ emit $ "(def " <> (visibility deft) <> ident <> "\n"
      , identVal val
      , return $ emit ")"
      ]
      where
        identVal :: (Emit gen) => Clj -> StateT PrinterState Maybe gen
        identVal clj = withIndent $ do
          clj' <- prettyPrintClj' clj
          indent' <- currentIndent
          return $ indent' <> clj'

        visibility :: DefType -> Text
        visibility TopPvt = "^:private "
        visibility _ = ""
    match (CljLet binds rets) = mconcat <$> sequence
      [ return $ emit "(let\n"
      , withIndent $ do
          entries <- forM binds prettyPrintClj'
          ind <- currentIndent
          return $ ind <> emit "[" <> (intercalate (emit "\n" <> ind <> emit " ") entries)
      , return $ emit "]\n"
      , withIndent $ do
          rets' <- forM rets prettyPrintClj'
          identString <- currentIndent
          return $ identString <> intercalate (emit "\n") rets'
      , return $ emit ")"
      ]
    match (CljCond conds el) = mconcat <$> sequence
      [ return $ emit "(cond\n"
      , intercalate (emit "\n") <$> forM conds printCond
      , return $ emit "\n"
      , maybe (return mempty) (\el' -> printCond (CljKeywordLiteral "else", el')) el
      , return $ emit ")"
      ]
      where
        printCond :: (Emit gen) => (Clj, Clj) -> StateT PrinterState Maybe gen
        printCond (check, val) = withIndent $ do
          check' <- prettyPrintClj' check
          val' <- prettyPrintClj' val
          identString <- currentIndent
          return $ identString <> check' <> emit " " <> val'
    match (CljIf cond then' else') = mconcat <$> sequence
      [ return $ emit "(if "
      , prettyPrintClj' cond
      , return $ emit "\n"
      , printIndented then'
      , return $ emit "\n"
      , printIndented else'
      , return $ emit ")"
      ]
      where
        printIndented :: (Emit gen) => Clj -> StateT PrinterState Maybe gen
        printIndented clj = withIndent $ do
          clj' <- prettyPrintClj' clj
          indentStr <- currentIndent
          return $ indentStr <> clj'
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
    match (CljNamespace ns comm req) = mconcat <$> sequence
      [ return $ emit $ "(ns " <> ns
      , return $ emit (maybe " " (\cm -> "\n  \"" <> cm <> "\"\n  ") comm)
      , maybe (return $ emit "") (\req' -> prettyPrintClj' req') req
      , return $ emit ")"
      ]
    match (CljFunction mname args ret) = mconcat <$> sequence
      [ return $ emit "(fn "
      , return $ emit $ (maybe "" (\name -> name <> " ") mname)
      , return $ emit $ "[" <> (intercalate " " args) <> "]\n"
      , withIndent $ do
          ret' <- prettyPrintClj' ret
          indentString <- currentIndent
          return $ indentString <> ret'
      , return $ emit ")"
      ]
    match (CljThrow throw) = do
      throw' <- prettyPrintClj' throw
      return $ emit "(throw " <> throw' <> emit ")"
    match (CljNoOp) = return $ emit ""
    match _ = mzero

accessor :: Pattern PrinterState Clj (Text, Clj)
accessor = mkPattern match
  where
  match (CljAccessor (KeyWord prop) val) = Just (T.cons ':' prop, val)
  match (CljAccessor (KeyStr prop) val) = Just ("\"" <> prop <> "\"", val)
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
    cljs <- traverse prettyPrintClj' args
    return (intercalate (emit " ") cljs, val)
  match (CljUnary Not val) = do
    cljs <- traverse prettyPrintClj' [val]
    return (intercalate (emit " ") cljs, CljVar Nothing "not")
  match (CljUnary Negate val) = do
    cljs <- traverse prettyPrintClj' [val]
    return (intercalate (emit " ") cljs, CljVar Nothing "-")
  match (CljBinary op vals) = do
    cljs <- traverse prettyPrintClj' vals
    return (intercalate (emit " ") cljs, CljVar Nothing (binaryTable op))
  match _ = mzero

binaryTable :: BinaryOperator -> Text
binaryTable = go where
  go Add = "+"
  go Subtract = "-"
  go Multiply = "*"
  go Divide = "/"
  go Modulus = "mod"
  go StringAppend = "str"
  go Equal = "="
  go NotEqual = "not="
  go LessThan = "<"
  go LessThanOrEqual = "<="
  go GreaterThan = ">"
  go GreaterThanOrEqual = ">="
  go And = "and"
  go Or = "or"
  go BitAnd = "bit-and"
  go BitOr = "bit-or"
  go BitXor = "bit-xor"
  go ShiftLeft = "bit-shift-left"
  go ShiftRight = "bit-shift-right"
  go UnsignedShiftRight = "unsigned-bit-shift-right"

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
    matchValue = buildPrettyPrinter operators (literals <+> matchValue)

    operators :: (Emit gen) => OperatorTable PrinterState Clj gen
    operators =
      OperatorTable [ [ Wrap accessor $ \prop val ->
                          emit "(get " <> val <> sp <> emit prop <> emit ")" ]
                    , [ Wrap indexer $ \index val ->
                          emit "(nth " <> val <> sp <> index <> emit ")" ]
                    , [ Wrap app $ \args val ->
                          emit "(" <> val <> sp <> args <> emit ")" ]
                    ]
      where
        sp :: (Emit gen) => gen
        sp = emit " "

keyToStr :: (Emit gen) => KeyType -> gen
keyToStr (KeyStr s) = emit $ prettyPrintString s
keyToStr (KeyWord k) = emit $ prettyPrintSymbol $ ":" <> k
