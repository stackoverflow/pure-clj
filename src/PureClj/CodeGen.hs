module PureClj.CodeGen where

import Prelude.Compat

import CoreFn
import PureClj.AST
import PureClj.Common
import PureClj.Optimizer

import Control.Arrow
import Control.Monad (replicateM, forM, zipWithM)
import Control.Monad.Supply.Class
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

nsComment :: Text
nsComment = "Generated with pure-clj"

moduleToClj :: forall m. (Monad m, MonadSupply m) => Module Ann -> m [Clj]
moduleToClj (Module _ mn _ imps exps foreigns decls) = do
  let imports = importToClj <$> (filter shouldImport imps)
      imports' = if null foreigns
                 then imports
                 else CljRequire (runModuleName mn <> "._foreign") "_foreign" : imports
      imports'' = if null imports'
                  then Nothing
                  else Just $ CljApp (CljKeywordLiteral "require") imports'
      namespace = CljNamespace (runModuleName mn <> "._core") (Just nsComment) imports''
      declares = makeDeclares decls
      foreigns' = makeForeignImport <$> foreigns
  definitions <- bindToClj True `mapM` decls
  let optimized = optimize <$> concat definitions
  return $ namespace : declares ++ foreigns' ++ optimized
  where
    shouldImport :: (Ann, ModuleName) -> Bool
    shouldImport (_, mn') = mn' /= mn && isNotPrim mn'
      where
        isNotPrim (ModuleName (p : _)) | p == (ProperName "Prim") = False
        isNotPrim _ = True

    importToClj :: (Ann, ModuleName) -> Clj
    importToClj (_,  mn') = CljRequire (name <> "._core") name
      where
        name = runModuleName mn'

    makeForeignImport :: Ident -> Clj
    makeForeignImport ident =
      CljDef (topType ident) (runIdent ident) (CljVar (Just "_foreign") (runIdent ident))

    makeDeclares :: [Bind Ann] -> [Clj]
    makeDeclares binds =
      let declares = concatMap declare binds
      in if null declares
         then []
         else [def declares]
      where
        declare :: Bind Ann -> [Ident]
        declare (NonRec _ ident _) = [ident]
        declare (Rec vals) = snd . fst <$> vals

        def idents = CljDeclare (identToClj <$> idents)

    bindToClj :: Bool -> Bind Ann -> m [Clj]
    bindToClj isTopLv (NonRec ann ident val) = return <$> exprToClj isTopLv (ann, ident) val
    bindToClj isTopLv (Rec vals) = forM vals (uncurry $ (exprToClj isTopLv))

    exprToClj :: Bool -> (Ann, Ident) -> Expr Ann -> m Clj
    exprToClj _ (_, (Ident "main")) expr
      | isMain mn = do
          expr' <- valToClj expr
          return $ CljFunction (Just "-main") ["& args"] (CljApp expr' [])
    exprToClj isTopLv (_, ident) expr = do
      let deft = if isTopLv then topType ident else LetDef
      expr' <- valToClj expr
      return $ CljDef deft (identToClj ident) expr'

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
      let args = unAbs lam
          identArgs = identToClj <$> args
      in return $ CljFunction Nothing identArgs (CljObjectLiteral $ toEntry <$> args)
      where
        unAbs :: Expr Ann -> [Ident]
        unAbs (Abs _ arg val) = arg : unAbs val
        unAbs _ = []

        toEntry :: Ident -> (KeyType, Clj)
        toEntry arg = (KeyStr (runIdent arg), var' (identToClj arg))
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
        _ -> flip (foldl (\fn a -> CljApp fn (makeParList a))) args' <$> valToClj f
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)

        makeParList :: Clj -> [Clj]
        makeParList (CljVar _ "undefined") = []
        makeParList x = [x]

    valToClj (Let _ bs val) = do
      ret <- valToClj val
      binds <- mapM (bindToClj False) bs
      return $ CljLet (concat binds) [ret]
    valToClj (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
      return $ CljDef LetDef (properToClj ctor) $
                 CljObjectLiteral [(KeyWord "create",
                                    CljFunction Nothing ["value"] (var' "value"))]
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
                    CljLet [CljDef LetDef "m" obj,
                            CljDef LetDef "create" createFn] [letBody]
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
      let letFn = CljLet (zipWith (CljDef LetDef) valNames vals)
          throw = CljThrow $ CljApp (var' "ex-info")
            [ CljStringLiteral "Exhausted pattern matching"
            , CljObjectLiteral []]
      cljs <- forM binders (processBinder valNames)
      return $ letFn [CljCond (concat cljs) (Just throw)]
      where
        processBinder :: [Text] -> CaseAlternative Ann -> m [(Clj, Clj)]
        processBinder valNames' (CaseAlternative bs res) = do
          ret <- guardToClj res
          condBinds <- zipWithM binderToClj valNames' bs
          --allConds <- zipWithM condToClj valNames' bs
          let cljAnd = CljBinary And
              (allConds, binds) = fold condBinds
              conds' = filter notTrue allConds
              conds = if null conds' then CljBooleanLiteral True else cljAnd conds'
              let' = CljLet binds
          case ret of
            -- | in case of guards
            Left ret' -> return $ (\(c, v) -> (cljAnd $ let' [c] : conds', let' [v])) <$> ret'
            -- | no guards
            Right ret' -> return [(conds, let' [ret'])]

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

    binderToClj :: Text -> Binder Ann -> m ([Clj], [Clj])
    binderToClj _ NullBinder{} = return ([CljBooleanLiteral True], [])
    binderToClj varName (LiteralBinder _ l) = literalBinderToClj varName l
    binderToClj varName (VarBinder _ ident) =
      return ([CljBooleanLiteral True], [letDef (identToClj ident) $ var' varName])
    binderToClj varName (NamedBinder _ ident binder) = do
      (innerCond, inner) <- binderToClj varName binder
      return $ (innerCond, (letDef (identToClj ident) $ var' varName) : inner)
    binderToClj varName (ConstructorBinder (_, _, _, Just IsNewtype) _ _ [binder]) =
      binderToClj varName binder
    binderToClj varName (ConstructorBinder (_, _, _, Just (IsConstructor ct fields)) _ ctor bs) = do
      defs <- zipWithM (go $ var' varName) fields bs
      let res@(conds, binds) = fold defs
      return $ case ct of
        ProductType -> res
        SumType -> ( CljBinary Equal [ accessType (var' varName)
                                     , accessType (qualifiedToClj (Ident . runProperName) ctor)] : conds
                   , binds)
      where
        accessType value = CljAccessor (KeyWord "type") value
        go :: Clj -> Ident -> Binder Ann -> m ([Clj], [Clj])
        go ctorVar ident bind = do
          argVar <- freshName
          (conds', cljs) <- binderToClj argVar bind
          let argClj = letDef argVar (CljAccessor (KeyWord . runIdent $ ident) ctorVar)
          let letCond = CljLet [argClj]
          return $ ([letCond conds'], argClj : cljs)
    binderToClj _ ConstructorBinder{} = error "binderToClj: invalid ConstructorBinder"

    literalBinderToClj :: Text -> Literal (Binder Ann) -> m ([Clj], [Clj])
    literalBinderToClj varName (NumericLiteral n) =
      return $ ([CljBinary Equal [(CljNumericLiteral n), (var' varName)]], [])
    literalBinderToClj varName (StringLiteral s) =
      return $ ([CljBinary Equal [(CljStringLiteral s), (var' varName)]], [])
    literalBinderToClj varName (CharLiteral c) =
      return $ ([CljBinary Equal [(CljCharLiteral c), (var' varName)]], [])
    literalBinderToClj varName (BooleanLiteral b) =
      return $ ([CljBinary Equal [(CljBooleanLiteral b), (var' varName)]], [])
    literalBinderToClj varName (ArrayLiteral bs) = do
      cljs <- zipWithM (arrayBinder varName) [0..] bs
      return $ fold cljs
      where
        arrayBinder :: Text -> Int -> Binder Ann -> m ([Clj], [Clj])
        arrayBinder _ _ NullBinder{} = return ([CljBooleanLiteral True], [])
        arrayBinder varName' i binder = do
          newVar <- freshName
          (conds, cljs') <- binderToClj newVar binder
          let accessor = (CljArrayIndexer (CljNumericLiteral $ Left i) (var' varName'))
              def = CljDef LetDef newVar accessor
              let' = case binder of
                       VarBinder{} -> [CljBooleanLiteral True]
                       _ -> [CljLet [CljDef LetDef newVar accessor] conds]
          return (let', def : cljs')

    literalBinderToClj varName (ObjectLiteral bs) = do
      cljs <- mapM (objBinder varName) bs
      return $ fold cljs
      where
        objBinder :: Text -> (Text, Binder Ann) -> m ([Clj], [Clj])
        objBinder _ (_, NullBinder{}) = return ([CljBooleanLiteral True], [])
        objBinder varName' (prop, binder) = do
          newVar <- freshName
          (conds, cljs') <- binderToClj newVar binder
          let accessor = (CljAccessor (KeyStr prop) $ var' varName')
              def = CljDef LetDef newVar accessor
              let' = case binder of
                       VarBinder{} -> [CljBooleanLiteral True]
                       _ -> [CljLet [CljDef LetDef newVar accessor] conds]
          return (let', def : cljs')

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
    letDef name val = CljDef LetDef name val

    topType :: Ident -> DefType
    topType ident | ident `elem` exps = Top
                  | otherwise = TopPvt

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

-- | Maps a tuple2 with the provided functions
mapT :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapT f g = f *** g
