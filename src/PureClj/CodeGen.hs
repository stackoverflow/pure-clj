module PureClj.CodeGen where

import CoreFn
import PureClj.AST
import PureClj.Common

import Control.Arrow
import qualified Data.Text as T

nsComment :: String
nsComment = "Generated with pure-clj"

moduleToClj :: Module Ann -> [Clj]
moduleToClj (Module coms mn path imps exps foreigns decls) =
  let namespace = CljApp (CljVar "ns") $ [CljVar $ runModuleName mn ++ ".core", CljStringLiteral nsComment]
      imports = importToClj <$> imps
      definitions = bindToClj `concatMap` decls
  in
    namespace : imports ++ definitions
  where
    importToClj :: (Ann, ModuleName) -> Clj
    importToClj (_,  mn') = CljApp (CljVar "require") [CljArrayLiteral [CljStringLiteral $ name ++ ".core :as " ++ name]]
      where
        name = runModuleName mn'

    bindToClj :: Bind Ann -> [Clj]
    bindToClj (NonRec ann ident val) = [exprToClj (ann, ident) val]
    bindToClj (Rec vals) = (uncurry exprToClj) <$> vals

    exprToClj :: (Ann, Ident) -> Expr Ann -> Clj
    exprToClj (_, (Ident "main")) expr
      | isMain mn = CljFunction (Just "-main") ["& args"] (CljApp (valToClj expr) [])
    exprToClj (_, ident) expr = CljVarIntroduction (identToClj ident) (Just (valToClj expr))

    valToClj :: Expr Ann -> Clj
    valToClj (Literal _ expr) = literalToClj expr
    valToClj (Var (_, _, _, (Just (IsConstructor _ []))) name) = qualifiedToClj name
    valToClj (Var (_, _, _, (Just (IsConstructor _ _))) name) =
      CljAccessorKeyword "create" (qualifiedToClj name)

    literalToClj :: Literal (Expr Ann) -> Clj
    literalToClj (NumericLiteral (Left i)) = CljNumericLiteral (Left i)
    literalToClj (NumericLiteral (Right d)) = CljNumericLiteral (Right d)
    literalToClj (StringLiteral s) = CljStringLiteral $ T.unpack s
    literalToClj (CharLiteral c) = CljCharLiteral c
    literalToClj (BooleanLiteral b) = CljBooleanLiteral b
    literalToClj (ArrayLiteral ar) = CljArrayLiteral $ valToClj <$> ar
    literalToClj (ObjectLiteral objs) = CljObjectLiteral $ mapT T.unpack valToClj <$> objs

    mapT :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
    mapT f g = f *** g

    qualifiedToClj :: Qualified Ident -> Clj
    qualifiedToClj (Qualified (Just (ModuleName [ProperName mn'])) a)
      | mn' == "Prim" = CljVar . runIdent $ a
    qualifiedToClj (Qualified (Just mn') a)
      | mn /= mn' = CljVar $ (runModuleName mn') ++ "/" ++ (identToClj a)
    qualifiedToClj (Qualified _ a) = CljVar $ identToClj a

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False
