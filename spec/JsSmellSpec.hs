module JsSmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "JS.hasRedundantLambda" $ do
    it "is True whn η-conversion applies" $ do
      hasRedundantLambda "x" (js "var x = function(m) { return f(m) }") `shouldBe` True

    it "is False when it is an unavoidable lambda" $ do
      hasRedundantLambda "x" (js "var x = function(m) { return m(f) }") `shouldBe` False

  describe "hasRedundantIf" $ do
    it "is True when both branches are boolean literal returns" $ do
      hasRedundantIf "x" (js "function x() { if(m) return true else return false }") `shouldBe` True
      hasRedundantIf "x" (js "function x() { if(m) return false else return true }") `shouldBe` True

    it "is True when return an if with boolean literals" $ do
      hasRedundantIf "x" (js "function x() { return m ? true : false }") `shouldBe` True

    it "is False when there is no if" $ do
      hasRedundantIf "x" (js "var x = false") `shouldBe` False

    it "is False when there are no literals" $ do
      hasRedundantIf "x" (js "function x() { if(m) 2 else 4 }") `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in an if" $ do
      hasRedundantBooleanComparison "x" (js "function x(m) { return m == true }") `shouldBe` True

    it "is False when no comparison" $ do
      hasRedundantBooleanComparison "x" (js "function x(m) { return m }") `shouldBe` False

  describe "returnsNull" $ do
    it "is True when returns null" $ do
      returnsNull "x" (js "function x(m) { return null }") `shouldBe` True

    it "is True when returns undefined" $ do
      returnsNull "x" (js "function x(m) { return undefined }") `shouldBe` True

    it "is False when returns a number" $ do
      returnsNull "x" (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesNullTest" $ do
    it "is True when tests for null" $ do
      doesNullTest "x" (js "function x(m) { if ( m == null) 1 else 2 } ") `shouldBe` True

    it "is True when tests for null with ===" $ do
      doesNullTest "x" (js "function x(m) { if ( m === null) 1 else 2 } ") `shouldBe` True

    it "is False when not does null test" $ do
      doesNullTest "x" (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesTypeTest" $ do
    it "is True when tests for string" $ do
      doesTypeTest "x" (js "function x(m) { if ( m == \"foo\") 1 else 2 } ") `shouldBe` True

    it "is True when tests for string flipped with ===" $ do
      doesTypeTest "x" (js "function x(m) { if ( \"foo\" === m) 1 else 2 } ") `shouldBe` True

    it "is False when not does type test" $ do
      doesTypeTest "x" (js "function x(m) { return 1 }") `shouldBe` False