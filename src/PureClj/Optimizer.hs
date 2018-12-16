module PureClj.Optimizer where

import Prelude.Compat

import Control.Monad.Supply.Class

import PureClj.AST
import PureClj.Optimizer.Common
import PureClj.Optimizer.Inliner
import PureClj.Optimizer.MagicDo
import PureClj.Optimizer.TCO (tco)

optimize :: MonadSupply m => Clj -> m Clj
optimize clj = do
  clj' <- untilFixedPoint ( inlineFnComposition . inlineUnsafeCoerce
                            . inlineUnsafePartial . tidyUp . applyAll
                            [ inlineCommonValues
                            , inlineCommonOps ]) clj
  untilFixedPoint (return . tidyUp) . tco
    =<< untilFixedPoint (return . magicDo')
    =<< untilFixedPoint (return . magicDo) clj'
  where
    tidyUp :: Clj -> Clj
    tidyUp = applyAll $
      [ fixLets
      , etaConvert
      , simplifyConds
      , inlineVariables
      ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'
