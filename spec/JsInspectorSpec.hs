module JsInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Parsers.JavaScript
import           Data.Maybe (fromJust)

js = fromJust.parseJavaScript

spec :: Spec
spec = do
  describe "hasFunctionDeclaration" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        hasFunctionDeclaration "f"  (js "function f(x) {return 1}") `shouldBe` True

      it "is False when functions is not declared" $ do
        hasFunctionDeclaration "g"  (js "function f(x) {return 1}") `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        hasFunctionDeclaration "f"  (js "var f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        hasFunctionDeclaration "f"  (js "var f = function(x) {}") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        hasFunctionDeclaration "f"  (js "var f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        hasFunctionDeclaration "f"  (js "var f = []") `shouldBe` False

      it "is True when constant is declared with a variable literal" $ do
        hasFunctionDeclaration "f"  (js "var f = setTimeout") `shouldBe` True

  describe "hasArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        hasArity 1 "f" (js "function(x) { return x + 1 }") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        hasArity 2 "f" (js "function(x) { x + 1}") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        hasArity 2 "f" (js "var f = function(x, y) { return x + y }") `shouldBe` True

  describe "hasObject" $ do
    it "is True when present" $ do
      hasObject "f" (js "var f = {x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      hasObject "f" (js "var f = 6")  `shouldBe` False

    it "is False when present on another binding" $ do
      hasObject "f" (js "var g = {}")  `shouldBe` False


