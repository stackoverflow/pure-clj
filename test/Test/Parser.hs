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
    it "correctly finds foreign definitions" $ do
      checkForeignsDefined
    it "incorrect foreign definitions should not be found" $ do
      checkForeignsNotDefined

checkParserWorks :: IO Bool
checkParserWorks = do
  file <- readFileUTF8asString "test/resources/sample.clj"
  let res = parseClojure file
  return $ isRight res

checkForeignsDefined :: Expectation
checkForeignsDefined =
  let clj = "(def x 1)\n(defn y 2)\n(def ^:public z 0)"
      parsed = parseClojure clj in
    case parsed of
      Left _ -> expectationFailure "Should have been parsed"
      Right cljs -> let checker = checkForeign cljs in
                      checker "x" && checker "y" && checker "z" `shouldBe` True

checkForeignsNotDefined :: Expectation
checkForeignsNotDefined =
  let clj = "(def xx 1)\n(defn- y 2)\n(def ^:private z 0)\n(def ^{:private true} w 0)"
      parsed = parseClojure clj in
    case parsed of
      Left _ -> expectationFailure "Should have been parsed"
      Right cljs -> let checker = checkForeign cljs in
        checker "x" || checker "y" || checker "z" || checker "w" `shouldBe` False
