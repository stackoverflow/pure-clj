{-# LANGUAGE RecordWildCards #-}

module PureClj.Optimizer.MagicDo where
{-
import Prelude.Compat

import qualified PureClj.Constants as C

import Data.Text (Text)

import PureClj.AST
import PureClj.Optimizer.Common

magicDo' :: Clj -> Clj
magicDo' = magicDo'' C.effect C.effectDictionaries

magicDo'' :: Text -> C.EffectDictionaries -> Clj -> Clj
magicDo'' effectModule C.EffectDictionaries{..} = everywhereTopDown convert
  where
  -- The name of the function block which is added to denote a do block
  fnName = "__do"
  -- Desugar monomorphic calls to >>= and return for the Eff monad
  convert :: Clj -> Clj
  -- Desugar pure
  convert (CljApp (CljApp pure' [val]) []) | isPure pure' = val
  -- Desugar discard
  convert (CljApp (CljApp bind [m]) [CljFunction Nothing [] clj]) | isDiscard bind =
    CljFunction (Just fnName) [] $ CljApp m [] : map applyReturns clj
  -- Desugar bind
  convert (App _ (App _ bind [m]) [Function s1 Nothing [arg] (Block s2 js)]) | isBind bind =
    Function s1 (Just fnName) [] $ Block s2 (VariableIntroduction s2 arg (Just (App s2 m [])) : map applyReturns js)
  -- Desugar untilE
  convert (App s1 (App _ f [arg]) []) | isEffFunc C.untilE f =
    App s1 (Function s1 Nothing [] (Block s1 [ While s1 (Unary s1 Not (App s1 arg [])) (Block s1 []), Return s1 $ ObjectLiteral s1 []])) []
  -- Desugar whileE
  convert (App _ (App _ (App s1 f [arg1]) [arg2]) []) | isEffFunc C.whileE f =
    App s1 (Function s1 Nothing [] (Block s1 [ While s1 (App s1 arg1 []) (Block s1 [ App s1 arg2 [] ]), Return s1 $ ObjectLiteral s1 []])) []
  -- Inline __do returns
  convert (Return _ (App _ (Function _ (Just ident) [] body) [])) | ident == fnName = body
  -- Inline double applications
  convert (App _ (App s1 (Function s2 Nothing [] (Block ss body)) []) []) =
    App s1 (Function s2 Nothing [] (Block ss (applyReturns `fmap` body))) []
  convert other = other
  -- Check if an expression represents a monomorphic call to >>= for the Eff monad
  isBind (App _ fn [dict]) | isDict (effectModule, edBindDict) dict && isBindPoly fn = True
  isBind _ = False
  -- Check if an expression represents a call to @discard@
  isDiscard (CljApp (CljApp fn [dict1]) [dict2])
    | isDict (C.controlBind, C.discardUnitDictionary) dict1 &&
      isDict (effectModule, edBindDict) dict2 &&
      isDiscardPoly fn = True
  isDiscard _ = False
  -- Check if an expression represents a monomorphic call to pure or return for the Eff applicative
  isPure (CljApp fn [dict]) | isDict (effectModule, edApplicativeDict) dict && isPurePoly fn = True
  isPure _ = False
  -- Check if an expression represents the polymorphic >>= function
  isBindPoly = isDict (C.controlBind, C.bind)
  -- Check if an expression represents the polymorphic pure function
  isPurePoly = isDict (C.controlApplicative, C.pure')
  -- Check if an expression represents the polymorphic discard function
  isDiscardPoly = isDict (C.controlBind, C.discard)
  -- Check if an expression represents a function in the Effect module
  isEffFunc name (CljVar (Just eff) name') = eff == effectModule && name == name'
  isEffFunc _ _ = False
-}
