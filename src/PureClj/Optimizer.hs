module PureClj.Optimizer where

import Prelude.Compat

import PureClj.AST
import PureClj.Optimizer.Common
import PureClj.Optimizer.Inliner

optimize :: Clj -> Clj
optimize = applyAll $
  [ nameFunctions
  , inlineCommonValues
  , inlineCommonOps
  ]
