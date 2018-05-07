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
  let namespace = CljApp (CljVar Nothing "ns") $
        [CljVar Nothing $ runModuleName mn ++ ".core", CljStringLiteral nsComment]
      imports = importToClj <$> imps
      definitions = bindToClj `concatMap` decls
  in
    namespace : imports ++ definitions
  where
    importToClj :: (Ann, ModuleName) -> Clj
    importToClj (_,  mn') = CljApp (CljVar Nothing "require") [CljArrayLiteral [CljStringLiteral $ name ++ ".core :as " ++ name]]
      where
        name = runModuleName mn'

    bindToClj :: Bind Ann -> [Clj]
    bindToClj (NonRec ann ident val) = [exprToClj (ann, ident) val]
    bindToClj (Rec vals) = (uncurry exprToClj) <$> vals

    exprToClj :: (Ann, Ident) -> Expr Ann -> Clj
    exprToClj (_, (Ident "main")) expr
      | isMain mn = CljFunction (Just "-main") ["& args"] (CljApp (valToClj expr) [])
    exprToClj (_, ident) expr = CljDef (identToClj ident) (Just (valToClj expr))

    valToClj :: Expr Ann -> Clj
    valToClj (Literal _ expr) = literalToClj expr
    valToClj (Var (_, _, _, (Just (IsConstructor _ []))) name) = qualifiedToClj name
    valToClj (Var (_, _, _, (Just (IsConstructor _ _))) name) =
      CljAccessor (KeyWord "create") (qualifiedToClj name)
    valToClj (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
      if mn == mn'
      then foreignIdent ident
      else varToClj qi
    valToClj (Var (_, _, _, Just IsForeign) ident) =
      error $ "Encountered a unqualified reference to a foreign ident " ++ (showQualified runIdent ident)
    valToClj (Var _ ident) = varToClj ident
    valToClj (Accessor _ prop val) = CljAccessor (KeyStr . T.unpack $  prop) $ valToClj val
    valToClj (ObjectUpdate _ o ps) =
      let obj = valToClj o
          ps' = mapT (KeyStr . T.unpack) valToClj <$> ps
      in CljObjectUpdate obj ps'
    valToClj lam@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
      let args = identToClj <$> unAbs lam
      in CljFunction Nothing args (CljObjectLiteral $ toEntry <$> args)
      where
        unAbs :: Expr Ann -> [Ident]
        unAbs (Abs _ arg val) = arg : unAbs val
        unAbs _ = []

        toEntry :: String -> (KeyType, Clj)
        toEntry arg = (KeyWord arg, CljVar Nothing arg)
    valToClj (Abs _ arg val) =
      let ret = valToClj val
          cljArgs = case arg of
            UnusedIdent -> []
            _           -> [identToClj arg]
      in CljFunction Nothing cljArgs ret
    valToClj ap@App{} =
      let (f, args) = unApp ap []
          args' = valToClj <$> args
      in case f of
        Var (_, _, _, Just IsNewtype) _ -> head args'
        Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
          CljApp (CljAccessor (KeyWord "create") (qualifiedToClj name)) args'
        Var (_, _, _, Just IsTypeClassConstructor) name ->
          CljApp (qualifiedToClj name) args'
        _ -> foldl (\fn a -> CljApp fn [a]) (valToClj f) args'
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)
    valToClj (Let _ bs val) = CljLet (concatMap bindToClj bs) (valToClj val)
    valToClj _ = CljStringLiteral "Placeholder -- delete me"

    literalToClj :: Literal (Expr Ann) -> Clj
    literalToClj (NumericLiteral (Left i)) = CljNumericLiteral (Left i)
    literalToClj (NumericLiteral (Right d)) = CljNumericLiteral (Right d)
    literalToClj (StringLiteral s) = CljStringLiteral $ T.unpack s
    literalToClj (CharLiteral c) = CljCharLiteral c
    literalToClj (BooleanLiteral b) = CljBooleanLiteral b
    literalToClj (ArrayLiteral ar) = CljArrayLiteral $ valToClj <$> ar
    literalToClj (ObjectLiteral objs) = CljObjectLiteral $ mapT (KeyStr . T.unpack) valToClj <$> objs

    -- | Generate a simple non-namespaced Clojure var
    var :: Ident -> Clj
    var = CljVar Nothing . identToClj

    varToClj :: Qualified Ident -> Clj
    varToClj (Qualified Nothing ident) = var ident
    varToClj qual = qualifiedToClj qual

    qualifiedToClj :: Qualified Ident -> Clj
    qualifiedToClj (Qualified (Just (ModuleName [ProperName mn'])) a)
      | mn' == "Prim" = CljVar Nothing . runIdent $ a
    qualifiedToClj (Qualified (Just mn') a)
      | mn /= mn' = CljVar (Just $ runModuleName mn') (identToClj a)
    qualifiedToClj (Qualified _ a) = CljVar Nothing $ identToClj a

    foreignIdent :: Ident -> Clj
    foreignIdent ident = CljVar (Just "$foreign") (runIdent ident)

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

-- | Maps a tuple2 with the provided functions
mapT :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapT f g = f *** g
