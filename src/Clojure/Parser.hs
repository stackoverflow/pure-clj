-- | Simple (and probably incomplete) Clojure parser
module Clojure.Parser where

import Prelude.Compat

import Text.ParserCombinators.Parsec hiding (spaces)

data CljVal
  -- | Literals
  = String String
  | Number String
  | Char Char
  | Nil
  | Boolean Bool
  | Keyword String
  | Symbol String
  | ParamName String
  -- | Data structures
  | List [CljVal]
  | Vector [CljVal]
  | Map [(CljVal, CljVal)]
  -- | Reader macros
  | Lambda [CljVal]
  | Metadata CljVal
  | Regex String
  | VarQuote CljVal
  | HostExpr CljVal
  | Set [CljVal]
  | Tag CljVal
  | Discard CljVal
  | Dispatch CljVal CljVal
  | Deref CljVal
  | Quote CljVal
  | Backtick CljVal
  | Unquote CljVal
  | UnquoteSplice CljVal
  | Gensym CljVal
  deriving (Read, Show, Eq)

spaces :: Parser ()
spaces = skipMany1 (space <|> char ',')

parseKeyword :: Parser CljVal
parseKeyword = do
  _ <- char ':'
  x <- many1 (noneOf " :/")
  return $ Keyword x

parseString' :: Parser String
parseString' = do
  x <- between (char '"') (char '"') $ many (noneOf "\"")
  return x

parseString :: Parser CljVal
parseString = do
  x <- parseString'
  return $ String x

parseRegex :: Parser CljVal
parseRegex = do
  _ <- char '#'
  x <- parseString'
  return $ Regex x

parseChar :: Parser CljVal
parseChar = do
  _ <- char '\\'
  x <- anyChar
  return $ Char x

parseParamName :: Parser CljVal
parseParamName = do
  _ <- char '%'
  x <- option "" $ (oneOf ['0'..'9'] <:> many digit) <|> string "&"
  return $ ParamName x

-- Numbers

parseNumber :: Parser CljVal
parseNumber = do
  x <- try parseBin <|> try parseHex <|> try parseFloat <|> parseLong
  return $ Number x
  where
    parseLong = longPlus <|> longMinus <|> long
      where
        long = try (int <* oneOf "lLnN") <|> int
        longMinus = char '-' <:> long
        longPlus = char '+' *> long
    parseFloat = try infinity <|> try nan <|> plus <|> minus <|> number
      where
        number :: Parser String
        number = int <++> ((char '.') <:> option "" int) <++> (option "" $ oneOf "eE" <:> (minusInt <|> int))
        plus = char '+' *> number
        minus = char '-' <:> number
        infinity :: Parser String
        infinity = string "-Infinity" <|> string "Infinity"
        nan :: Parser String
        nan = string "-NaN" <|> string "NaN"
    parseHex = char '0' <:> (oneOf "xX" <:> many1 hexDigit)
    parseBin = char '0' <:> (oneOf "bB" <:> many1 (oneOf "01"))
    int = many1 digit
    minusInt = char '-' <:> int

-- Symbols

parseName :: Parser String
parseName = parseHead <:> (option "" $ many parseRest <++> (option "" $ char ';' <:> many1 parseRest))
  where
    parseHead :: Parser Char
    parseHead = noneOf "0123456789^`'\"#~@:/%()[]{}\n\r\t \\"
    parseRest :: Parser Char
    parseRest = parseHead <|> digit <|> char '.'

parseSymbol :: Parser CljVal
parseSymbol = do
  x <- try (parseName <++> string "/" <++> parseSymbol') <|> parseSymbol'
  return $ case x of
    "nil" -> Nil
    "true" -> Boolean True
    "false" -> Boolean False
    a -> Symbol a
  where
    parseSymbol' = string "." <|> string "/" <|> parseName

-- Data structures

parseList' :: Parser [CljVal]
parseList' = do
  xs <- sepBy parseDef spaces
  return xs

parseList :: Parser CljVal
parseList = do
  _ <- char '('
  xs <- parseList'
  _ <- char ')'
  return $ List xs

parseVector :: Parser CljVal
parseVector = do
  _ <- char '['
  xs <- parseList'
  _ <- char ']'
  return $ Vector xs

parseMap :: Parser CljVal
parseMap = do
  _ <- char '{'
  kvs <- sepBy parseKV spaces
  _ <- char '}'
  return $ Map kvs
  where
    parseKV = do
      k <- parseDef
      _ <- spaces
      v <- parseDef
      return (k, v)

parseSet :: Parser CljVal
parseSet = do
  _ <- string "#{"
  xs <- parseList'
  _ <- char '}'
  return $ Set xs

parseLambda :: Parser CljVal
parseLambda = do
  _ <- string "#("
  xs <- parseList'
  _ <- char ')'
  return $ Lambda xs

parseMetadata :: Parser CljVal
parseMetadata = do
  _ <- string "^" <|> string "#^"
  x <- parseMap
  return $ Metadata x

parseTag :: Parser CljVal
parseTag = do
  _ <- string "^" <|> string "#^"
  x <- parseSymbol <|> parseKeyword <|> parseString
  return $ Tag x

parseVarQuote :: Parser CljVal
parseVarQuote = do
  _ <- string "#'"
  x <- parseSymbol
  return $ VarQuote x

parseHostExpr :: Parser CljVal
parseHostExpr = do
  _ <- string "#+"
  x <- parseSymbol
  return $ HostExpr x

parseDiscard :: Parser CljVal
parseDiscard = do
  _ <- string "#_"
  x <- parseDef
  return $ Discard x

parseDeref :: Parser CljVal
parseDeref = do
  _ <- char '@'
  x <- parseDef
  return $ Deref x

parseQuote :: Parser CljVal
parseQuote = do
  _ <- char '\''
  x <- parseDef
  return $ Quote x

parseUnquote :: Parser CljVal
parseUnquote = do
  _ <- char '~'
  x <- parseDef
  return $ Unquote x

parseBacktick :: Parser CljVal
parseBacktick = do
  _ <- char '`'
  x <- parseDef
  return $ Backtick x

parseUnquoteSplice :: Parser CljVal
parseUnquoteSplice = do
  _ <- string "~@"
  x <- parseDef
  return $ UnquoteSplice x

parseGensym :: Parser CljVal
parseGensym = undefined

parseHashes :: Parser CljVal
parseHashes = try parseSet
  <|> try parseLambda
  <|> try parseVarQuote
  <|> try parseHostExpr
  <|> try parseDiscard
  <|> try parseMetadata
  <|> try parseTag
  <|> try parseRegex

parseTildes :: Parser CljVal
parseTildes = try parseUnquoteSplice
  <|> try parseUnquote

parseDef :: Parser CljVal
parseDef = try parseNumber
  <|> parseSymbol
  <|> parseString
  <|> parseKeyword
  <|> parseChar
  <|> parseParamName
  <|> parseList
  <|> parseVector
  <|> parseMap
  <|> parseHashes
  <|> parseTildes
  <|> parseDeref
  <|> parseQuote
  <|> parseBacktick

parseClojure :: String -> Either ParseError [CljVal]
parseClojure s = parse (many1 parseDef) "" s

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b
