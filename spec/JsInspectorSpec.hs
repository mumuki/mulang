module JsInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Parsers.JavaScript

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

      it "is False when is a method" $ do
        hasFunctionDeclaration "f"  (js "var o = {f: function(){}}") `shouldBe` False

  describe "hasArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        hasArity 1 "f" (js "function f(x) { return x + 1 }") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        hasArity 2 "f" (js "function f(x) { x + 1}") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        hasArity 2 "f" (js "var f = function(x, y) { return x + y }") `shouldBe` True

  describe "hasWhile" $ do
    it "is True when present in function" $ do
      hasWhile "f" (js "function f() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in lambda" $ do
      hasWhile "f" (js "var f = function() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in object" $ do
      hasWhile "x" (js "var x = {f: function() { while(true) { console.log('foo') } }}")  `shouldBe` True

    it "is True when present in method" $ do
      hasWhile "o.f" (js "var o = {f: function() { while(true) { console.log('foo') }  }}")  `shouldBe` True

    it "is False when not present in function" $ do
      hasWhile "f" (js "function f() {}")  `shouldBe` False

  describe "hasObject" $ do
    it "is True when present" $ do
      hasObject "f" (js "var f = {x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      hasObject "f" (js "var f = 6")  `shouldBe` False

    it "is False when present on another binding" $ do
      hasObject "f" (js "var g = {}")  `shouldBe` False

  describe "hasMethod" $ do
    it "is True when present" $ do
      hasMethod "x" "f" (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when not present" $ do
      hasMethod "m" "f" (js "var f = {x: function(){}}")  `shouldBe` False

    it "is False when not a method" $ do
      hasMethod "m" "f" (js "var f = {x: 6}")  `shouldBe` False

    it "is False when object not present" $ do
      hasMethod "x" "p" (js "var f = {x: function(){}}")  `shouldBe` False

  describe "hasAttribute" $ do
    it "is True when present" $ do
      hasAttribute "x" "f" (js "var f = {x: 6}")  `shouldBe` True

    it "is True when present and there are many" $ do
      hasAttribute "x" "f" (js "var f = {j: 20, x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      hasAttribute "m" "f" (js "var f = {x: 6}")  `shouldBe` False

    it "is False when object not present" $ do
      hasAttribute "x" "p" (js "var f = {x: 6}")  `shouldBe` False

  describe "hasUsage" $ do
    it "is True on direct usage in function" $ do
      hasUsage "m" "f" (js "function f(x) { m }") `shouldBe` True

    it "is True through function application in function" $ do
      hasUsage "m" "f" (js "function g() { m }; function f(x) { g() }") `shouldBe` True

    it "is True through function application in function" $ do
      hasUsage "m" "f" (js "function g() { m }; function f(x) { g() }") `shouldBe` True

    it "is False through function application in function" $ do
      hasUsage "m" "f" (js "function g() { m }; function f(x) { k() }") `shouldBe` False

    it "is True through message send in function" $ do
      hasUsage "m" "f" (js "var o = {g: function(){ m }}; function f(x) { o.g() }") `shouldBe` True

    it "is True through message send in objects" $ do
      hasUsage "m" "p" (js "var o = {g: function(){ m }}; var p = {n: function() { o.g() }}") `shouldBe` True

    it "is False through message send in objects" $ do
      hasUsage "m" "p" (js "var o = {g: function(){ m }}; var y = {n: function() { o.g() }}") `shouldBe` False

    it "is True through message send in objects, with nested binding" $ do
      hasUsage "m" "p.n" (js "var o = {g: function(){ m }}; var p = {n: function() { o.g() }}") `shouldBe` True

    it "is False through message send in objects, with nested binding" $ do
      hasUsage "m" "p.w" (js "var o = {g: function(){ m }}; var p = {n: function() { o.g() }}") `shouldBe` False