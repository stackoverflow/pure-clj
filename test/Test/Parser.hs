module Test.Parser where

import Prelude.Compat

import Data.Either (isRight)
import Test.Hspec

import Clojure.Parser
import System.IO.UTF8 (readFileUTF8asString)

parserTest :: Spec
parserTest =
  describe "Parse Clojure code" $ do
    it "parse Clojure without failling" $ do
      checkParserWorks `shouldReturn` True
    it "parse Clojure correctly" $ do
      let clj = "(def ^:private foo 9)" in
        parseClojure clj `shouldBe` (Right [List [Symbol "def", Tag (Keyword "private"), Symbol "foo", Number "9"]])

checkParserWorks :: IO Bool
checkParserWorks = do
  file <- readFileUTF8asString "test/resources/sample.clj"
  let res = parseClojure file
  return $ isRight res
