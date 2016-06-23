module JsSmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "JS.hasRedundantLambda" $ do
    it "is True whn etha-conversion applies" $ do
      hasRedundantLambda (js "(function(m) { return f(m) })") `shouldBe` True

    it "is False when it is an unavoidable lambda" $ do
      hasRedundantLambda (js "(function(m) { return m(f) })") `shouldBe` False

  describe "hasRedundantIf" $ do
    it "is True when both branches are boolean literal returns" $ do
      hasRedundantIf (js "function x() { if(m) return true else return false }") `shouldBe` True
      hasRedundantIf (js "function x() { if(m) return false else return true }") `shouldBe` True

    it "is True when return an if with boolean literals" $ do
      hasRedundantIf (js "function x() { return m ? true : false }") `shouldBe` True

    it "is True when return an if with boolean literals, top level" $ do
      hasRedundantIf (js "m ? true : false") `shouldBe` True

    it "is True when return an if with boolean literals, in method" $ do
      hasRedundantIf (js "var y = {x: function(m){ return m ? true : false }}") `shouldBe` True

    it "is False when there is no if" $ do
      hasRedundantIf (js "var x = false") `shouldBe` False

    it "is False when there are no literals" $ do
      hasRedundantIf (js "function x() { if(m) 2 else 4 }") `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in a function" $ do
      hasRedundantBooleanComparison (js "function x(m) { return m == true }") `shouldBe` True

    it "is True when comparing a literal, top level" $ do
      hasRedundantBooleanComparison (js "m == true") `shouldBe` True

    it "is False when no comparison" $ do
      hasRedundantBooleanComparison (js "function x(m) { return m }") `shouldBe` False

  describe "hasRedundantLocalVariableReturn" $ do
    it "is True when local variable is not necessary" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x  = 5; return x; }") `shouldBe` True

    it "is False when local variable is not necessary, but there are many variables" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; var y = 2; return x; }") `shouldBe` False

    it "is False when local variable is necessary in return" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; return x + x; }") `shouldBe` False

    it "is False when local variable is updated" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; x+= 1; return x; }") `shouldBe` False

    it "is False when local variable is used as a cache" $ do
      hasRedundantLocalVariableReturn (js "function x(m) { var x = 5; var y = 1 + x; g(y); return x; }") `shouldBe` False

  describe "hasAssignmentReturn" $ do
    it "is True when return contains assignment" $ do
      hasAssignmentReturn (js "function x(m) { return x = 4 }") `shouldBe` True

    it "is False when return does not contain assignment" $ do
      hasAssignmentReturn (js "function x(m) { return x == 4 }") `shouldBe` False

  describe "returnsNull" $ do
    it "is True when returns null" $ do
      returnsNull (js "function x(m) { return null }") `shouldBe` True

    it "is True when returns undefined" $ do
      returnsNull (js "function x(m) { return undefined }") `shouldBe` True

    it "is False when returns a number" $ do
      returnsNull (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesNullTest" $ do
    it "is True when tests for null" $ do
      doesNullTest (js "function x(m) { if ( m == null) 1 else 2 } ") `shouldBe` True

    it "is True when tests for null with ===" $ do
      doesNullTest (js "function x(m) { if ( m === null) 1 else 2 } ") `shouldBe` True

    it "is False when not does null test" $ do
      doesNullTest (js "function x(m) { return 1 }") `shouldBe` False

  describe "doesTypeTest" $ do
    it "is True when tests for string" $ do
      doesTypeTest (js "function x(m) { if ( m == \"foo\") 1 else 2 } ") `shouldBe` True

    it "is True when tests for string flipped with ===" $ do
      doesTypeTest (js "function x(m) { if ( \"foo\" === m) 1 else 2 } ") `shouldBe` True

    it "is False when not does type test" $ do
      doesTypeTest (js "function x(m) { return 1 }") `shouldBe` False
