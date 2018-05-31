module PureClj.Optimizer where

import Prelude.Compat

import Control.Monad.Supply.Class

import PureClj.AST
import PureClj.Optimizer.Common
import PureClj.Optimizer.Inliner
--import PureClj.Optimizer.MagicDo

optimize :: MonadSupply m => Clj -> m Clj
optimize clj = do
  clj' <- untilFixedPoint ( inlineFnComposition . inlineUnsafeCoerce
                            . inlineUnsafePartial . tidyUp . applyAll
                            [ inlineCommonValues
                            , inlineCommonOps ]) clj
  untilFixedPoint (return . tidyUp) clj'
    -- =<< untilFixedPoint (return . magicDo') clj'
  where
    tidyUp :: Clj -> Clj
    tidyUp = applyAll $
      [ nameLets
      , etaConvert
      , inlineVariables
      ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
