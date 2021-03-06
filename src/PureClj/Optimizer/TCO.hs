module PureClj.Optimizer.TCO where

import Prelude.Compat

import Data.Monoid ((<>))
import Data.Text (Text)
import PureClj.AST

-- | Change tail calls to use `recur`
tco :: Clj -> Clj
tco = everywhere convert where
  convert :: Clj -> Clj
  convert (CljDef typ ident fn@(CljFunction _ _ _))
    | isTailRecursive ident body'
    = CljDef typ ident $ replace (tcoify ident argss body')
    where
      argss = concat . reverse $ args
      (args, replace, body') = collectFunctionArgs [] id fn
  convert other = other

  tcoVar :: Text -> Text
  tcoVar name = "$tcovar_" <> name

  collectFunctionArgs :: [[Text]] -> (Clj -> Clj) -> Clj -> ([[Text]], (Clj -> Clj), Clj)
  collectFunctionArgs args f (CljFunction name pars [body]) =
    collectFunctionArgs (pars : args) (\b -> f (CljFunction name (map tcoVar pars) [b])) body
  collectFunctionArgs args f body = (args, f, body)

  isTailRecursive :: Text -> Clj -> Bool
  isTailRecursive ident clj = countSelfReferences clj > 0 && allInTailPosition clj where
    countSelfReferences = everything (+) match where
      match :: Clj -> Int
      match (CljVar Nothing ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition :: Clj -> Bool
    allInTailPosition (CljLet defs [body]) =
      all ((== 0) . countSelfReferences) defs && allInTailPosition body
    allInTailPosition (CljCond conds els) =
      all ((== 0) . countSelfReferences) (fst <$> conds)
      && all allInTailPosition (snd <$> conds)
      && all allInTailPosition els
    allInTailPosition (CljIf cond the els) =
      countSelfReferences cond == 0 && allInTailPosition the && allInTailPosition els
    allInTailPosition (CljArrayLiteral arr) = all ((== 0) . countSelfReferences) arr
    allInTailPosition (CljArrayIndexer p1 p2) = countSelfReferences p1 == 0 && countSelfReferences p2 == 0
    allInTailPosition (CljObjectLiteral kvs) = all ((== 0) . countSelfReferences) (map snd kvs)
    allInTailPosition (CljAccessor _ obj) = countSelfReferences obj == 0
    allInTailPosition (CljObjectUpdate obj kvs) = countSelfReferences obj == 0 && all ((== 0) . countSelfReferences) (map snd kvs)
    allInTailPosition (CljUnary _ par) = countSelfReferences par == 0
    allInTailPosition (CljBinary _ pars) = all ((== 0) . countSelfReferences) pars
    allInTailPosition (CljBooleanLiteral _) = True
    allInTailPosition (CljCharLiteral _) = True
    allInTailPosition (CljKeywordLiteral _) = True
    allInTailPosition (CljNumericLiteral _) = True
    allInTailPosition (CljStringLiteral _) = True
    allInTailPosition (CljVar Nothing ident')
      | ident' == ident = False
      | otherwise = True
    allInTailPosition (CljThrow expr) =
      countSelfReferences expr == 0
    allInTailPosition fnapp@(CljApp _ _)
      | isSelfCall ident fnapp = countSelfReferences fnapp == 1
      | otherwise = countSelfReferences fnapp == 0
    allInTailPosition _ = False

  tcoify :: Text -> [Text] -> Clj -> Clj
  tcoify name args body =
    CljLet [CljDef LetDef fname
            (CljFunction Nothing args
             [(recur name body)])]
    [CljApp (var' fname) (map (var' . tcoVar) args)]
    where
      fname = "$tcofun_" <> name
      var' name' = CljVar Nothing name'
      recur :: Text -> Clj -> Clj
      recur ident = everywhereTopDown go where
        go :: Clj -> Clj
        go fn@(CljApp _ _) | isSelfCall ident fn
          = CljApp (var' "recur") $ concat (collectPars [] fn)
        go other = other

  collectPars :: [[Clj]] -> Clj -> [[Clj]]
  collectPars acc (CljApp fn pars) = collectPars (pars : acc) fn
  collectPars acc _ = acc

  isSelfCall :: Text -> Clj -> Bool
  isSelfCall ident (CljApp (CljVar Nothing ident') _) = ident == ident'
  isSelfCall ident (CljApp fn _) = isSelfCall ident fn
  isSelfCall _ _ = False
