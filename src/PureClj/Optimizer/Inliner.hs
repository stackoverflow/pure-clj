module PureClj.Optimizer.Inliner where

import Prelude.Compat

import qualified PureClj.Constants as C

import PureClj.AST
import PureClj.Optimizer.Common

shouldInline :: Clj -> Bool
shouldInline (CljVar _ _) = True
shouldInline (CljNumericLiteral _) = True
shouldInline (CljStringLiteral _) = True
shouldInline (CljBooleanLiteral _) = True
shouldInline (CljArrayIndexer index val) = shouldInline index && shouldInline val
shouldInline (CljAccessor _ val) = shouldInline val
shouldInline _ = False

inlineCommonOps :: Clj -> Clj
inlineCommonOps = everywhereTopDown $ applyAll $
  []

-- | give names to functions defined in a `let`
-- | so they can be called recursively
nameFunctions :: Clj -> Clj
nameFunctions = everywhere name
  where
    name :: Clj -> Clj
    name (CljDef LetDef var (CljFunction Nothing pars bd)) =
      CljDef LetDef var (CljFunction (Just var) pars bd)
    name x = x
