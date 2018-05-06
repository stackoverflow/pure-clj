{-# LANGUAGE TemplateHaskell #-}

module CoreFn.Ann where

import CoreFn.Meta (Meta)
import CoreFn.Names (Qualified, ProperName, OpName)
import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.Aeson.TH

newtype SkolemScope = SkolemScope Int
  deriving (Show, Eq, Ord)

data SourcePos = SourcePos
  { sourcePosLine :: Int
  , sourcePosColumn :: Int
  } deriving (Show, Eq, Ord)

data SourceSpan = SourceSpan
  { spanName :: String
  , spanStart :: SourcePos
  , spanEnd :: SourcePos
  } deriving (Show, Eq, Ord)

data Comment
  = LineComment String
  | BlockComment String
  deriving (Show, Eq, Ord, Read)

newtype Label = Label String
  deriving (Show, Eq, Ord)

data Kind
  = KUnknown Int
  | Row Kind
  | FunKind Kind Kind
  | NamedKind (Qualified ProperName)
  deriving (Show, Eq, Ord)

data Type
  = TUnknown Int
  | TypeVar String
  | TypeLevelString String
  | TypeWildcard SourceSpan
  | TypeConstructor (Qualified ProperName)
  | TypeOp (Qualified OpName)
  | TypeApp Type Type
  | ForAll String Type (Maybe SkolemScope)
  | ConstrainedType Constraint Type
  | Skolem String Int SkolemScope (Maybe SourceSpan)
  | REmpty
  | RCons Label Type Type
  | KindedType Type Kind
  | PrettyPrintFunction Type Type
  | PrettyPrintObject Type
  | PrettyPrintForAll [String] Type
  | BinaryNoParensType Type Type Type
  | ParensInType Type
  deriving (Show, Eq, Ord)

data ConstraintData
  = PartialConstraintData [[String]] Bool
  deriving (Show, Eq, Ord)

data Constraint = Constraint
  { constraintClass :: Qualified ProperName
  , constraintArgs  :: [Type]
  , constraintData  :: Maybe ConstraintData
  } deriving (Show, Eq, Ord)

type Ann = (SourceSpan, [Comment], Maybe Type, Maybe Meta)

-- instances
instance A.FromJSON SourcePos where
  parseJSON arr = do
    [line, col] <- A.parseJSON arr
    return $ SourcePos line col

instance A.FromJSON SourceSpan where
  parseJSON = A.withObject "SourceSpan" $ \o ->
    SourceSpan <$>
    o .: "name" <*>
    o .: "start" <*>
    o .: "end"

$(deriveJSON (defaultOptions { sumEncoding = ObjectWithSingleField }) ''Comment)
