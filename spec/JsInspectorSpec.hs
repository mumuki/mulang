module JsInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "declaresFunction" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        declaresFunction "f" (js "function f(x) {return 1}") `shouldBe` True

      it "is True when functions is declared" $ do
        declaresFunction "f" (js "function f(x) {return 1}") `shouldBe` True

      it "is False when functions is not declared" $ do
        declaresFunction "g" (js "function f(x) {return 1}") `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction "f" (js "var f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        declaresFunction "f" (js "var f = function(x) {}") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        declaresFunction  "f" (js "var f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        declaresFunction "f" (js "var f = []") `shouldBe` False

      it "is False when is a method" $ do
        declaresFunction "f" (js "var o = {f: function(){}}") `shouldBe` False

  describe "declaresWithArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresWithArity 1) "f" (js "function f(x) { return x + 1 }") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresWithArity 2) "f" (js "function f(x) { x + 1}") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresWithArity 2) "f" (js "var f = function(x, y) { return x + y }") `shouldBe` True

  describe "usesWhile" $ do
    it "is True when present in function" $ do
      usesWhile (js "function f() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in lambda" $ do
      usesWhile (js "var f = function() { while(true) { console.log('foo') }  }")  `shouldBe` True

    it "is True when present in object" $ do
      usesWhile (js "var x = {f: function() { while(true) { console.log('foo') } }}")  `shouldBe` True

    it "is True when present in method" $ do
      usesWhile (js "var o = {f: function() { while(true) { console.log('foo') }  }}")  `shouldBe` True

    it "is False when not present in function" $ do
      usesWhile (js "function f() {}")  `shouldBe` False

  describe "declaresObject" $ do
    it "is True when present" $ do
      declaresObject "f"  (js "var f = {x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresObject "f" (js "var f = 6")  `shouldBe` False

    it "is False when not present, scoped" $ do
      declaresObject "f" (js "var g = {}")  `shouldBe` False

    it "is True when present, scoped" $ do
      declaresObject "g" (js "var g = {}")  `shouldBe` True

  describe "declaresMethod" $ do
    it "is True when present" $ do
      declaresMethod "x" (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when not present" $ do
      declaresMethod "m" (js "var f = {x: function(){}}")  `shouldBe` False

    it "is False when not a method" $ do
      declaresMethod "m" (js "var f = {x: 6}")  `shouldBe` False

    it "is True when object present, scoped" $ do
      scoped (declaresMethod "x") "f"  (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when object not present, scoped" $ do
      scoped (declaresMethod "x") "p"  (js "var f = {x: function(){}}")  `shouldBe` False

  describe "declaresAttribute" $ do
    it "is True when present" $ do
      declaresAttribute "x" (js "var f = {x: 6}")  `shouldBe` True

    it "is True when present and there are many" $ do
      declaresAttribute "x" (js "var f = {j: 20, x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresAttribute "m" (js "var f = {x: 6}")  `shouldBe` False

    it "is True when attribute present, scoped" $ do
      scoped (declaresAttribute "x") "f"  (js "var f = {x: 6}")  `shouldBe` True

    it "is False when attribute not present, scoped" $ do
      scoped (declaresAttribute "x") "g" (js "var f = {x: 6}")  `shouldBe` False

  describe "uses" $ do
    it "is True on direct usage in function" $ do
      uses "m" (js "function f(x) { m }") `shouldBe` True

    it "is True on direct usage in method" $ do
      uses "m" (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is True on direct usage in method, scoped" $ do
      scoped (uses "m") "o" (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped" $ do
      scoped (uses "p") "o" (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is True on usage in method, scoped twice" $ do
      scopedList (uses "m") ["o", "z"] (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped twice" $ do
      scopedList (uses "p") ["o", "z"] (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is False on usage in wrong method, scoped twice" $ do
      scopedList (uses "m") ["o", "z"] (js "var o = {p: function(x) { m }}") `shouldBe` False

    it "is True through function application in function" $ do
      transitive (uses "m") "f" (js "function g() { m }; function f(x) { g() }") `shouldBe` True

    it "is True through function application in function" $ do
      transitive (uses "m") "f" (js "function g(p) { return m }; function f(x) { return g(2) }") `shouldBe` True

    it "is False through function application in function" $ do
      transitive (uses "m") "f" (js "function g() { m }; function f(x) { k() }") `shouldBe` False

    it "is True through message send in function" $ do
      transitive (uses "m") "f" (js "var o = {g: function(){ m }}; function f(x) { o.g() }") `shouldBe` True

    it "is True through message send in objects" $ do
      transitive (uses "m") "p" (js "var o = {g: function(){ m }}\n\
                                        \var p = {n: function() { o.g() }}") `shouldBe` True

