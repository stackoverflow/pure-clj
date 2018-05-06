module CoreFn.Expr where

import Control.Arrow ((***))
import CoreFn.Binders (Binder)
import CoreFn.Ident (Ident)
import CoreFn.Literal (Literal)
import CoreFn.Names (ProperName, Qualified)
import Data.Text (Text)

data Expr a
  = Literal a (Literal (Expr a))
  | Constructor a ProperName ProperName [Ident]
  | Accessor a Text (Expr a)
  | ObjectUpdate a (Expr a) [(Text, (Expr a))]
  | Abs a Ident (Expr a)
  | App a (Expr a) (Expr a)
  | Var a (Qualified Ident)
  | Case a [Expr a] [CaseAlternative a]
  | Let a [Bind a] (Expr a)
  deriving (Eq, Show, Functor)

data Bind a
  = NonRec a Ident (Expr a)
  | Rec [((a, Ident), Expr a)]
  deriving (Eq, Show, Functor)

type Guard a = Expr a

data CaseAlternative a = CaseAlternative
  { caseAlternativeBinders :: [Binder a]
  , casealternativeResult :: Either [(Guard a, Expr a)] (Expr a)
  } deriving (Eq, Show)

instance Functor CaseAlternative where
  fmap f (CaseAlternative cabs car) = CaseAlternative
    (fmap (fmap f) cabs)
    (either (Left . fmap (fmap f *** fmap f)) (Right . fmap f) car)
