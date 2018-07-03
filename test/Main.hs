{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Prelude.Compat

import Control.Monad.Supply
import Data.Text (Text)
import Test.Hspec

import Test.Parser
import PureClj.AST
import PureClj.Optimizer

main :: IO ()
main = hspec $ do
  describe "Inline let variables" $ do
    it "inline the right variables" $ do
      checkInlines
    it "doesn't inline reassigned variables" $ do
      checkReassign
  parserTest

checkInlines :: Expectation
checkInlines =
  let ex = CljDef LetDef "z" (CljApp (key "value") [var "Nothing"])
      ex2 = CljDef LetDef "y" (CljApp (var "value") [var "Nothing"])
      let' = CljLet [ex, ex2] [var "z"]
      expected = CljLet [ex2] [CljApp (key "value") [var "Nothing"]]
  in (optimize' let') `shouldBe` expected

checkReassign :: Expectation
checkReassign =
  let ex = CljDef LetDef "z" (CljStringLiteral "a")
      ex2 = CljDef LetDef "z" (CljApp (var "value") [var "Nothing"])
      let' = CljLet [ex, ex2] [var "z"]
  in (optimize' let') `shouldBe` let'

optimize' :: Clj -> Clj
optimize' v = evalSupply 0 $ optimize v

var :: Text -> Clj
var x = CljVar Nothing x

key :: Text -> Clj
key x = CljKeywordLiteral x
