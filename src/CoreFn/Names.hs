module CoreFn.Names where

import qualified Data.Text as T

newtype ProperName = ProperName T.Text
  deriving (Eq, Ord, Show)

newtype ModuleName = ModuleName [ProperName]
  deriving (Eq, Ord, Show)

data Qualified a = Qualified (Maybe ModuleName) a
  deriving (Eq, Ord, Show)
