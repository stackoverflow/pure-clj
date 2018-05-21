module PureClj.CodeGen where

import Prelude.Compat

import CoreFn
import PureClj.AST
import PureClj.Common

import Control.Arrow
import Control.Monad (replicateM, forM, zipWithM)
import Control.Monad.Supply.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

nsComment :: Text
nsComment = "Generated with pure-clj"

moduleToClj :: forall m. (Monad m, MonadSupply m) => Module Ann -> m [Clj]
moduleToClj (Module coms mn path imps exps foreigns decls) = do
  let imports = importToClj <$> (filter shouldImport imps)
      imports' = case imports of
                   [] -> Nothing
                   _ -> Just $ CljApp (CljKeywordLiteral "require") imports
      namespace = CljNamespace (runModuleName mn <> ".core") (Just nsComment) imports'
      declares = makeDeclares decls
  definitions <- bindToClj True `mapM` decls
  return $ namespace : declares ++ (concat definitions)
  where
    shouldImport :: (Ann, ModuleName) -> Bool
    shouldImport (_, mn') = mn' /= mn && mn' /= ModuleName [ProperName "Prim"]

    importToClj :: (Ann, ModuleName) -> Clj
    importToClj (_,  mn') = CljRequire (name <> ".core") name
      where
        name = runModuleName mn'

    makeDeclares :: [Bind Ann] -> [Clj]
    makeDeclares binds = concatMap declare binds
      where
        declare :: Bind Ann -> [Clj]
        declare (NonRec _ ident _) = def ident
        declare (Rec vals) = concatMap def (snd . fst <$> vals)

        def ident = [CljDef True (identToClj ident) Nothing]

    bindToClj :: Bool -> Bind Ann -> m [Clj]
    bindToClj isTopLv (NonRec ann ident val) = return <$> exprToClj isTopLv (ann, ident) val
    bindToClj isTopLv (Rec vals) = forM vals (uncurry $ (exprToClj isTopLv))

    exprToClj :: Bool -> (Ann, Ident) -> Expr Ann -> m Clj
    exprToClj _ (_, (Ident "main")) expr
      | isMain mn = do
          expr' <- valToClj expr
          return $ CljFunction (Just "-main") ["& args"] (CljApp expr' [])
    exprToClj isTopLv (_, ident) expr = do
      expr' <- valToClj expr
      return $ CljDef isTopLv (identToClj ident) (Just expr')

    valToClj :: Expr Ann -> m Clj
    valToClj (Literal _ expr) = literalToClj expr
    valToClj (Var (_, _, _, (Just (IsConstructor _ []))) name) = return $ qualifiedToClj id name
    valToClj (Var (_, _, _, (Just (IsConstructor _ _))) name) =
      return $ CljAccessor (KeyWord "create") (qualifiedToClj id name)
    valToClj (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
      return $ if mn == mn'
               then var ident
               else varToClj qi
    valToClj (Var (_, _, _, Just IsForeign) ident) =
      error $ T.unpack $ "Encountered a unqualified reference to a foreign ident " <> (showQualified runIdent ident)
    valToClj (Var _ ident) = return $ varToClj ident
    valToClj (Accessor _ prop val) = CljAccessor (KeyStr prop) <$> valToClj val
    valToClj (ObjectUpdate _ o ps) = do
      obj <- valToClj o
      let keys = KeyStr <$> (map fst ps)
      vals <- mapM valToClj (map snd ps)
      let ps' = zip keys vals
      return $ CljObjectUpdate obj ps'
    valToClj lam@(Abs (_, _, _, Just IsTypeClassConstructor) _ _) =
      let args = identToClj <$> unAbs lam
      in return $ CljFunction Nothing args (CljObjectLiteral $ toEntry <$> args)
      where
        unAbs :: Expr Ann -> [Ident]
        unAbs (Abs _ arg val) = arg : unAbs val
        unAbs _ = []

        toEntry :: Text -> (KeyType, Clj)
        toEntry arg = (KeyWord arg, var' arg)
    valToClj (Abs _ arg val) = do
      ret <- valToClj val
      let cljArgs = case arg of
            UnusedIdent -> []
            _           -> [identToClj arg]
      return $ CljFunction Nothing cljArgs ret
    valToClj ap@App{} = do
      let (f, args) = unApp ap []
      args' <- mapM valToClj args
      case f of
        Var (_, _, _, Just IsNewtype) _ -> return $ head args'
        Var (_, _, _, Just (IsConstructor _ fields)) name | length args == length fields ->
          return $ CljApp (CljAccessor (KeyWord "create") (qualifiedToClj id name)) args'
        Var (_, _, _, Just IsTypeClassConstructor) name ->
          return $ CljApp (qualifiedToClj id name) args'
        _ -> flip (foldl (\fn a -> CljApp fn [a])) args' <$> valToClj f
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)
    valToClj (Let _ bs val) = do
      ret <- valToClj val
      binds <- mapM (bindToClj False) bs
      return $ CljLet (concat binds) [ret]
    valToClj (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
      return $ CljDef False (properToClj ctor) (Just $
                 CljObjectLiteral [(KeyWord "create",
                                    CljFunction Nothing ["value"] (var' "value"))])
    valToClj (Constructor _ _ (ProperName ctor) []) =
      return $ CljFunction Nothing [] (CljObjectLiteral [(KeyWord "type",
                                                          CljKeywordLiteral (properToClj ctor))])
    valToClj (Constructor _ _ (ProperName ctor) fields) =
      let vars = (identToClj <$> fields)
          obj = CljObjectLiteral $ [(KeyWord "type", CljKeywordLiteral (properToClj ctor))]
          letBody = CljObjectUpdate (var' "m")
                    [(KeyWord "create", var' "create")]
          body = CljObjectUpdate (var' "m") $
                 (KeyWord "create", var' "create") : [(KeyWord v, var' v) | v <- vars]
          createFn = foldr (\f inner -> CljFunction (nameCtorF "create" f) [f] inner) body vars
      in return $ CljFunction Nothing [] $
                    CljLet [CljDef False "m" $ Just obj,
                            CljDef False "create" $ Just createFn] [letBody]
      where
        nameCtorF :: Text -> Text -> Maybe Text
        nameCtorF name var'' | "value0" == var'' = Just name
        nameCtorF _ _ = Nothing
    valToClj (Case _ values binders) = do
      vals <- mapM valToClj values
      bindersToClj binders vals

    literalToClj :: Literal (Expr Ann) -> m Clj
    literalToClj (NumericLiteral (Left i)) = return $ CljNumericLiteral (Left i)
    literalToClj (NumericLiteral (Right d)) = return $ CljNumericLiteral (Right d)
    literalToClj (StringLiteral s) = return $ CljStringLiteral s
    literalToClj (CharLiteral c) = return $ CljCharLiteral c
    literalToClj (BooleanLiteral b) = return $ CljBooleanLiteral b
    literalToClj (ArrayLiteral ar) = CljArrayLiteral <$> mapM valToClj ar
    literalToClj (ObjectLiteral objs) = do
      vals <- mapM valToClj $ snd <$> objs
      let keys = map KeyStr $ fst <$> objs
      return $ CljObjectLiteral (zip keys vals)

    bindersToClj :: [CaseAlternative Ann] -> [Clj] -> m Clj
    bindersToClj binders vals = do
      valNames <- replicateM (length vals) freshName
      let letFn = CljLet (zipWith (CljDef False) valNames (Just <$> vals))
          throw = CljThrow $ CljApp (var' "ex-info")
            [ CljStringLiteral "Exhausted pattern matching"
            , CljObjectLiteral []]
      cljs <- forM binders $ \(CaseAlternative bs res) -> do
        ret <- guardToClj res
        binds <- zipWithM binderToClj valNames bs
        allConds <- zipWithM condToClj valNames bs
        let cljAnd = CljBinary And
            conds' = filter notTrue (concat allConds)
            conds = if null conds' then CljBooleanLiteral True else cljAnd conds'
            let' = CljLet (concat binds)
        case ret of
          Left ret' -> return $ (\(c, v) -> (cljAnd $ let' [c] : conds', let' [v])) <$> ret'
          Right ret' -> return [(conds, let' [ret'])]
      return $ letFn [CljCond (concat cljs) (Just throw)]
      where
        notTrue :: Clj -> Bool
        notTrue (CljBooleanLiteral True) = False
        notTrue _ = True

        guardToClj :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> m (Either [(Clj, Clj)] Clj)
        guardToClj (Left gs) = Left <$> traverse genGuard gs
          where
            genGuard (cond, val) = do
              cond' <- valToClj cond
              val' <- valToClj val
              return (cond', val')
        guardToClj (Right v) = Right <$> valToClj v

    binderToClj :: Text -> Binder Ann -> m [Clj]
    binderToClj _ NullBinder{} = return []
    binderToClj varName (LiteralBinder _ l) = literalBinderToClj varName l
    binderToClj varName (VarBinder _ ident) =
      return [letDef (identToClj ident) $ var' varName]
    binderToClj varName (NamedBinder _ ident binder) = do
      inner <- binderToClj varName binder
      return $ (letDef (identToClj ident) $ var' varName) : inner
    binderToClj varName (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [binder]) =
      binderToClj varName binder
    binderToClj varName (ConstructorBinder (_, _, _, Just (IsConstructor ct fields)) _ _ bs) = do
      -- | FIXME: check constructor type
      defs <- zipWithM (go $ var' varName) fields bs
      return $ concat defs
      where
        go :: Clj -> Ident -> Binder Ann -> m [Clj]
        go ctorVar ident bind = do
          argVar <- freshName
          clj <- binderToClj argVar bind
          let argClj = letDef argVar (CljAccessor (KeyWord . runIdent $ ident) ctorVar)
          return $ argClj : clj
    binderToClj _ ConstructorBinder{} = error "binderToClj: invalid ConstructorBinder"

    literalBinderToClj :: Text -> Literal (Binder Ann) -> m [Clj]
    literalBinderToClj varName (ArrayLiteral bs) = do
      cljs <- zipWithM (arrayBinder varName) [0..] bs
      return $ concat cljs
      where
        arrayBinder :: Text -> Int -> Binder Ann -> m [Clj]
        arrayBinder _ _ NullBinder{} = return []
        arrayBinder varName' i binder = do
          newVar <- freshName
          clj <- binderToClj newVar binder
          let accessor = (CljArrayIndexer (CljNumericLiteral $ Left i) (var' varName'))
              def = CljDef False newVar (Just accessor)
          return $ def : clj

    literalBinderToClj varName (ObjectLiteral bs) = do
      cljs <- mapM (objBinder varName) bs
      return $ concat cljs
      where
        objBinder :: Text -> (Text, Binder Ann) -> m [Clj]
        objBinder _ (_, NullBinder{}) = return []
        objBinder varName' (prop, binder) = do
          newVar <- freshName
          clj <- binderToClj newVar binder
          let accessor = (CljAccessor (KeyStr prop) $ var' varName')
              def = CljDef False newVar (Just accessor)
          return $ def : clj

    literalBinderToClj _ _ = return []

    condToClj :: Text -> Binder Ann -> m [Clj]
    condToClj _ NullBinder{} = return $ [CljBooleanLiteral True]
    condToClj _ VarBinder{} = return $ [CljBooleanLiteral True]
    condToClj val (LiteralBinder _ l) = literalCondToClj val l
    condToClj val (NamedBinder _ _ binder) = condToClj val binder
    condToClj val (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [binder]) =
      condToClj val binder
    condToClj val (ConstructorBinder (_, _, _, Just (IsConstructor _ _)) _ ctor _) =
      return $ [CljBinary Equal [ accessType (var' val)
                                , accessType (qualifiedToClj (Ident . runProperName) ctor)]]
      where
        accessType :: Clj -> Clj
        accessType value = CljAccessor (KeyWord "type") value
    condToClj _ ConstructorBinder{} = error "condToClj: invalid ConstructorBinder"

    literalCondToClj :: Text -> Literal (Binder Ann) -> m [Clj]
    literalCondToClj varName (NumericLiteral n) =
      return $ [CljBinary Equal [(CljNumericLiteral n), (var' varName)]]
    literalCondToClj varName (StringLiteral s) =
      return $ [CljBinary Equal [(CljStringLiteral s), (var' varName)]]
    literalCondToClj varName (CharLiteral c) =
      return $ [CljBinary Equal [(CljCharLiteral c), (var' varName)]]
    literalCondToClj varName (BooleanLiteral b) =
      return $ [CljBinary Equal [(CljBooleanLiteral b), (var' varName)]]
    literalCondToClj varName (ArrayLiteral bs) = do
      cljs <- zipWithM (arrayCond varName) [0..] bs
      let lengthCheck = CljBinary Equal [ CljNumericLiteral $ Left (length bs)
                                        , CljApp (var' "count") [var' varName]]
      return $ lengthCheck : cljs
      where
        arrayCond :: Text -> Int -> Binder Ann -> m Clj
        arrayCond _ _ NullBinder{} = return $ CljBooleanLiteral True
        arrayCond _ _ VarBinder{} = return $ CljBooleanLiteral True
        arrayCond varName' i binder = do
          newVar <- freshName
          cljs' <- condToClj newVar binder
          let accessor = (CljArrayIndexer (CljNumericLiteral $ Left i) (var' varName'))
              let' = CljLet [CljDef False newVar (Just accessor)]
          return $ let' cljs'

    literalCondToClj varName (ObjectLiteral obj) = mapM (objCond varName) obj
      where
        objCond :: Text -> (Text, Binder Ann) -> m Clj
        objCond _ (_, NullBinder{}) = return $ CljBooleanLiteral True
        objCond _ (_, VarBinder{}) = return $ CljBooleanLiteral True
        objCond varName' (prop, binder) = do
          newVar <- freshName
          cljs' <- condToClj newVar binder
          let accessor = (CljAccessor (KeyStr prop) $ var' varName')
              let' = CljLet [CljDef False newVar (Just accessor)]
          return $ let' cljs'

    -- | Generate a simple non-namespaced Clojure var
    var :: Ident -> Clj
    var = CljVar Nothing . identToClj

    var' :: Text -> Clj
    var' name = CljVar Nothing name

    varToClj :: Qualified Ident -> Clj
    varToClj (Qualified Nothing ident) = var ident
    varToClj qual = qualifiedToClj id qual

    qualifiedToClj :: (a -> Ident) -> Qualified a -> Clj
    qualifiedToClj f (Qualified (Just (ModuleName [ProperName mn'])) a)
      | mn' == "Prim" = CljVar Nothing . runIdent $ f a
    qualifiedToClj f (Qualified (Just mn') a)
      | mn /= mn' = CljVar (Just $ runModuleName mn') (identToClj $ f a)
    qualifiedToClj f (Qualified _ a) = CljVar Nothing $ identToClj $ f a

    letDef :: Text -> Clj -> Clj
    letDef name val = CljDef False name (Just val)

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

-- | Maps a tuple2 with the provided functions
mapT :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapT f g = f *** g
