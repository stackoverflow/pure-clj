module PureClj.Optimizer.Common where

import Prelude.Compat

import Data.List (foldl')

applyAll :: [a -> a] -> a -> a
applyAll = foldl' (.) id
