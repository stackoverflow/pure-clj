module CoreFn.Names where

import Prelude.Compat

import CoreFn.Ident

import Data.Monoid ((<>))
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

runProperName :: ProperName -> Text
runProperName (ProperName name) = name

runModuleName :: ModuleName -> Text
runModuleName (ModuleName pns) = T.intercalate "." $ runProperName <$> pns

runModuleNamePath :: ModuleName -> [FilePath]
runModuleNamePath (ModuleName pns) = map T.unpack $ runProperName <$> pns

runIdent :: Ident -> Text
runIdent (Ident i) = i
runIdent (GenIdent Nothing n) = "$" <> T.pack (show n)
runIdent (GenIdent (Just name) n) = "$" <> name <> T.pack (show n)
runIdent UnusedIdent = error "UnusedIdent at runIdent"

showQualified :: (a -> Text) -> Qualified a -> Text
showQualified f (Qualified (Just mn) ident) =
  runModuleName mn <> "/" <> (f ident)
showQualified f (Qualified _ ident) = f ident
