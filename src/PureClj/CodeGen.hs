module PureClj.CodeGen where

import CoreFn
import PureClj.AST
import PureClj.Common

import Control.Arrow
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

nsComment :: Text
nsComment = "Generated with pure-clj"

moduleToClj :: Module Ann -> [Clj]
moduleToClj (Module coms mn path imps exps foreigns decls) =
  let namespace = CljApp (CljVar Nothing "ns") $
        [CljVar Nothing $ runModuleName mn <> ".core", CljStringLiteral nsComment]
      imports = importToClj <$> imps
      definitions = bindToClj True `concatMap` decls
  in
    namespace : imports ++ definitions
  where
    importToClj :: (Ann, ModuleName) -> Clj
    importToClj (_,  mn') = CljApp (CljVar Nothing "require") [CljArrayLiteral [CljStringLiteral $ name <> ".core :as " <> name]]
      where
        name = runModuleName mn'

    bindToClj :: Bool -> Bind Ann -> [Clj]
    bindToClj isTopLv (NonRec ann ident val) = [exprToClj isTopLv (ann, ident) val]
    bindToClj isTopLv (Rec vals) = (uncurry (exprToClj isTopLv)) <$> vals

    exprToClj :: Bool -> (Ann, Ident) -> Expr Ann -> Clj
    exprToClj _ (_, (Ident "main")) expr
      | isMain mn = CljFunction (Just "-main") ["& args"] (CljApp (valToClj expr) [])
    exprToClj isTopLv (_, ident) expr = CljDef isTopLv (identToClj ident) (Just (valToClj expr))

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
      error $ T.unpack $ "Encountered a unqualified reference to a foreign ident " <> (showQualified runIdent ident)
    valToClj (Var _ ident) = varToClj ident
    valToClj (Accessor _ prop val) = CljAccessor (KeyStr prop) $ valToClj val
    valToClj (ObjectUpdate _ o ps) =
      let obj = valToClj o
          ps' = mapT KeyStr valToClj <$> ps
      in CljObjectUpdate obj ps'
    valToClj lam@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
      let args = identToClj <$> unAbs lam
      in CljFunction Nothing args (CljObjectLiteral $ toEntry <$> args)
      where
        unAbs :: Expr Ann -> [Ident]
        unAbs (Abs _ arg val) = arg : unAbs val
        unAbs _ = []

        toEntry :: Text -> (KeyType, Clj)
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
    valToClj (Let _ bs val) = CljLet (concatMap (bindToClj False) bs) (valToClj val)
    valToClj (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
      CljDef False (properToClj ctor) (Just $
        CljObjectLiteral [(KeyWord "create",
          CljFunction Nothing ["value"] (CljVar Nothing "value"))])
    valToClj (Constructor _ _ (ProperName ctor) []) =
      CljFunction Nothing [] (CljObjectLiteral [(KeyWord "type",
                                                 CljKeywordLiteral (properToClj ctor))])
    valToClj (Constructor _ _ (ProperName ctor) fields) =
      let vars = (identToClj <$> fields)
          obj = CljObjectLiteral $ [(KeyWord "type", CljKeywordLiteral (properToClj ctor))]
          letBody = CljObjectUpdate (CljVar Nothing "m")
                    [(KeyWord "create", CljVar Nothing "create")]
          body = CljObjectUpdate (CljVar Nothing "m") $
                 (KeyWord "create", CljVar Nothing "create") : [(KeyWord v, CljVar Nothing v) | v <- vars]
          createFn = foldr (\f inner -> CljFunction (nameF "create" vars) [f] inner) body vars
      in CljFunction Nothing [] $
           CljLet [CljDef False "m" $ Just obj,
                   CljDef False "create" $ Just createFn] letBody
      where
        nameF :: Text -> [Text] -> Maybe Text
        nameF name vars | name == (head vars) = Just name
        nameF _ _ = Nothing
    valToClj _ = CljStringLiteral "Placeholder -- delete me"

    literalToClj :: Literal (Expr Ann) -> Clj
    literalToClj (NumericLiteral (Left i)) = CljNumericLiteral (Left i)
    literalToClj (NumericLiteral (Right d)) = CljNumericLiteral (Right d)
    literalToClj (StringLiteral s) = CljStringLiteral s
    literalToClj (CharLiteral c) = CljCharLiteral c
    literalToClj (BooleanLiteral b) = CljBooleanLiteral b
    literalToClj (ArrayLiteral ar) = CljArrayLiteral $ valToClj <$> ar
    literalToClj (ObjectLiteral objs) = CljObjectLiteral $ mapT KeyStr valToClj <$> objs

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
