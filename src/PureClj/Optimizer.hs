module PureClj.Optimizer where

import Prelude.Compat

import Control.Monad.Supply.Class

import PureClj.AST
import PureClj.Optimizer.Common
import PureClj.Optimizer.Inliner
--import PureClj.Optimizer.MagicDo
import Data.Text (Text)
import PureClj.Printer

optimize :: MonadSupply m => Clj -> m Clj
optimize clj = do
  clj' <- untilFixedPoint (inlineFnComposition . inlineUnsafeCoerce . inlineUnsafePartial) clj
  untilFixedPoint (return . tidyUp) clj'
    -- =<< untilFixedPoint (return . magicDo') clj'
  where
    tidyUp :: Clj -> Clj
    tidyUp = applyAll $
      [ inlineCommonValues
      , inlineCommonOps
      , nameFunctions
      , etaConvert
      , inlineVariables
      ]

untilFixedPoint :: (Monad m, Eq a) => (a -> m a) -> a -> m a
untilFixedPoint f = go
  where
  go a = do
   a' <- f a
   if a' == a then return a' else go a'

var :: Text -> Clj
var x = CljVar Nothing x

key :: Text -> Clj
key x = CljKeywordLiteral x

app :: Text -> Clj -> Clj -> Clj
app x y z = CljApp (var x) [y, z]

def :: Text -> Text -> Clj
def x y = CljDef LetDef x (var y)

example :: Clj
example = CljFunction Nothing ["v"] $
            CljLet [def "$0" "v"]
              [CljCond [ (app "instance?" (CljApp (key "class") [(var "Jus")]) (var "$0")
                         , (CljLet [def "$1" "$0"]
                            [CljApp (key "value0") [var "$1"]]))]
                Nothing]
