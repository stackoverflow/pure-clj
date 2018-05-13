module PureClj.CodeGen where

import CoreFn
import PureClj.AST
import PureClj.Common

import Control.Arrow
import Control.Monad (replicateM, forM)
import Control.Monad.Supply.Class
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T

nsComment :: Text
nsComment = "Generated with pure-clj"

moduleToClj :: forall m. (Monad m, MonadSupply m) => Module Ann -> m [Clj]
moduleToClj (Module coms mn path imps exps foreigns decls) = do
  let imports = importToClj <$> imps
      namespace = CljNamespace (runModuleName mn <> ".core") (Just nsComment) $
                  (Just $ CljApp (CljKeywordLiteral "require") imports)
  definitions <- bindToClj True `mapM` decls
  return $ namespace : (concat definitions)
  where
    importToClj :: (Ann, ModuleName) -> Clj
    importToClj (_,  mn') = CljRequire (name <> ".core") name
      where
        name = runModuleName mn'

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
    valToClj (Var (_, _, _, (Just (IsConstructor _ []))) name) = return $ qualifiedToClj name
    valToClj (Var (_, _, _, (Just (IsConstructor _ _))) name) =
      return $ CljAccessor (KeyWord "create") (qualifiedToClj name)
    valToClj (Var (_, _, _, Just IsForeign) qi@(Qualified (Just mn') ident)) =
      return $ if mn == mn'
               then CljVar Nothing $ identToClj ident
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
        toEntry arg = (KeyWord arg, CljVar Nothing arg)
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
          return $ CljApp (CljAccessor (KeyWord "create") (qualifiedToClj name)) args'
        Var (_, _, _, Just IsTypeClassConstructor) name ->
          return $ CljApp (qualifiedToClj name) args'
        _ -> flip (foldl (\fn a -> CljApp fn [a])) args' <$> valToClj f
      where
        unApp :: Expr Ann -> [Expr Ann] -> (Expr Ann, [Expr Ann])
        unApp (App _ val arg) args = unApp val (arg : args)
        unApp other args = (other, args)
    valToClj (Let _ bs val) = do
      ret <- valToClj val
      binds <- mapM (bindToClj False) bs
      return $ CljLet (concat binds) ret
    valToClj (Constructor (_, _, _, Just IsNewtype) _ (ProperName ctor) _) =
      return $ CljDef False (properToClj ctor) (Just $
                 CljObjectLiteral [(KeyWord "create",
                                    CljFunction Nothing ["value"] (CljVar Nothing "value"))])
    valToClj (Constructor _ _ (ProperName ctor) []) =
      return $ CljFunction Nothing [] (CljObjectLiteral [(KeyWord "type",
                                                          CljKeywordLiteral (properToClj ctor))])
    valToClj (Constructor _ _ (ProperName ctor) fields) =
      let vars = (identToClj <$> fields)
          obj = CljObjectLiteral $ [(KeyWord "type", CljKeywordLiteral (properToClj ctor))]
          letBody = CljObjectUpdate (CljVar Nothing "m")
                    [(KeyWord "create", CljVar Nothing "create")]
          body = CljObjectUpdate (CljVar Nothing "m") $
                 (KeyWord "create", CljVar Nothing "create") : [(KeyWord v, CljVar Nothing v) | v <- vars]
          createFn = foldr (\f inner -> CljFunction (nameF "create" vars) [f] inner) body vars
      in return $ CljFunction Nothing [] $
                    CljLet [CljDef False "m" $ Just obj,
                            CljDef False "create" $ Just createFn] letBody
      where
        nameF :: Text -> [Text] -> Maybe Text
        nameF name vars | name == (head vars) = Just name
        nameF _ _ = Nothing
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
      return $ CljLet (zipWith (CljDef False) valNames (Just <$> vals)) (CljArrayLiteral [])
      where
        guardToClj :: Either [(Guard Ann, Expr Ann)] (Expr Ann) -> Clj -> m [Clj]
        guardToClj (Left gs) next = traverse genGuard gs
          where
            genGuard (cond, val) = do
              cond' <- valToClj cond
              val' <- valToClj val
              return $ CljIfElse cond' val' (Just next)
        guardToClj (Right v) next = return [next]

    binderToClj :: [Text] -> [Clj] -> Binder Ann -> m [Clj]
    binderToClj _ done NullBinder{} = return done
    binderToClj varName done (LiteralBinder _ l) = undefined

    literalToBinderClj :: Text -> [Clj] -> Literal (Binder Ann) -> [Clj]
    literalToBinderClj varName done (NumericLiteral num) = undefined

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

isMain :: ModuleName -> Bool
isMain (ModuleName [ProperName "Main"]) = True
isMain _ = False

-- | Maps a tuple2 with the provided functions
mapT :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapT f g = f *** g
