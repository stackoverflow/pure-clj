module PureClj.Common where

import CoreFn.Ident

import Data.Char
import qualified Data.Text as T

identToClj :: Ident -> String
identToClj (Ident ident) = case T.unpack ident of
  name | isNameReserved name || isBuiltIn name -> "!!" ++ name
  name -> concatMap identCharToString name
identToClj (GenIdent _ _) = error "GenIdent in identToClj"
identToClj UnusedIdent = error "UnusedIdent in identToClj"

-- | Clojurizes an operator
identCharToString :: Char -> String
identCharToString c = case c of
  ch | isAlphaNum ch -> [ch]
  '_' -> "_"
  '.' -> "!dot"
  '$' -> "!dollar"
  '~' -> "!tilde"
  '=' -> "="
  '<' -> "<"
  '>' -> ">"
  '!' -> "!"
  '#' -> "!hash"
  '%' -> "%"
  '^' -> "!up"
  '&' -> "&"
  '|' -> "|"
  '*' -> "!times"
  '/' -> "/"
  '+' -> "+"
  '-' -> "-"
  ':' -> "!colon"
  '\\' -> "!bslash"
  '?' -> "?"
  '@' -> "!at"
  '\'' -> "*"
  ch -> '!' : show (ord ch)

-- | Checks whether a name is reserved in Clojure
isNameReserved :: String -> Bool
isNameReserved n = n `elem` cljReserved

cljReserved :: [String]
cljReserved =
  [ "def"
  , "if"
  , "do"
  , "let"
  , "quote"
  , "var"
  , "fn"
  , "loop"
  , "recur"
  , "throw"
  , "try"
  , "new"
  , "set!"
  , "nil"
  ]

-- | Checks if a name exists in Clojure
isBuiltIn :: String -> Bool
isBuiltIn name =
  isBuiltInOp name || elem name
  [ "defn"
  , "defrecord"
  , "deftype"
  , "defprotocol"
  , "defmethod"
  , "defmulti"
  , "defmacro"
  , "not="
  , "or"
  , "and"
  , "when"
  , "while"
  , "for"
  , "map"
  , "filter"
  , "reduce"
  , "quot"
  , "rem"
  , "mod"
  , "inc"
  , "dec"
  , "max"
  , "min"
  ]

-- | Checks if an operator exists in Clojure
isBuiltInOp :: String -> Bool
isBuiltInOp op = elem op
  [ "="
  , "=="
  , "+"
  , "-"
  , "*"
  , "/"
  , ">"
  , ">="
  , "<"
  , "<="
  ]
