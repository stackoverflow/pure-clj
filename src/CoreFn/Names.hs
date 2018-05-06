module CoreFn.Names where

import CoreFn.Ident

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T

newtype ProperName = ProperName Text
  deriving (Show, Ord, Eq)

newtype ModuleName = ModuleName [ProperName]
  deriving (Eq, Ord, Show)

data Qualified a = Qualified (Maybe ModuleName) a
  deriving (Eq, Ord, Show)

newtype OpName = OpName String
  deriving (Eq, Ord, Show)

runModuleName :: ModuleName -> String
runModuleName (ModuleName pns) = intercalate "." $ pstrs
  where
    pstrs = T.unpack . unwrap <$> pns
    unwrap (ProperName p) = p

runIdent :: Ident -> String
runIdent (Ident i) = T.unpack i
runIdent (GenIdent Nothing n) = "$" ++ show n
runIdent (GenIdent (Just name) n) = "$" ++ name ++ show n
runIdent UnusedIdent = error "UnusedIdent at runIdent"
