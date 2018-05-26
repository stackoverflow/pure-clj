module PureClj.Common where

import Prelude.Compat

import CoreFn.Ident

import Data.Char
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

identToClj :: Ident -> Text
identToClj (Ident ident) = properToClj ident
identToClj (GenIdent _ _) = error "GenIdent in identToClj"
identToClj UnusedIdent = "!__unused"

properToClj :: Text -> Text
properToClj name
  | isNameReserved name || isBuiltIn name = "!!" <> name
  | otherwise = T.concatMap identCharToText name

-- | Clojurizes an operator
identCharToText :: Char -> Text
identCharToText c = case c of
  ch | isAlphaNum ch -> T.singleton ch
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
  ch -> '!' `T.cons` T.pack (show (ord ch))

-- | Checks whether a name is reserved in Clojure
isNameReserved :: Text -> Bool
isNameReserved n = n `elem` cljReserved

cljReserved :: [Text]
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
isBuiltIn :: Text -> Bool
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
  , "void"
  , "Void"
  ]

-- | Checks if an operator exists in Clojure
isBuiltInOp :: Text -> Bool
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
