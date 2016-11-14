module JsInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector
import           Language.Mulang.Inspector.Combiner
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Inspector.Smell

spec :: Spec
spec = do
  describe "declaresFunction" $ do
    describe "with function declarations" $ do
      it "is True when functions is declared" $ do
        declaresFunction (named "f") (js "function f(x) {return 1}") `shouldBe` True

      it "is True when functions is declared" $ do
        declaresFunction (named "f") (js "function f(x) {return 1}") `shouldBe` True

      it "is True when any functions is declared" $ do
        declaresFunction anyone (js "function f(x) {return 1}") `shouldBe` True

      it "is False when functions is not declared" $ do
        declaresFunction (named "g") (js "function f(x) {return 1}") `shouldBe` False

    describe "with variables" $ do
      it "is False when constant is declared with a non lambda literal" $ do
        declaresFunction (named "f") (js "var f = 2") `shouldBe` False

      it "is True when constant is declared with a lambda literal" $ do
        declaresFunction (named "f") (js "var f = function(x) {}") `shouldBe` True

      it "is False when constant is declared with a number literal" $ do
        declaresFunction  (named "f") (js "var f = 3") `shouldBe` False

      it "is False when constant is declared with a list literal" $ do
        declaresFunction (named "f") (js "var f = []") `shouldBe` False

      it "is False when is a method" $ do
        declaresFunction (named "f") (js "var o = {f: function(){}}") `shouldBe` False

  describe "declaresComputationWithExactArity" $ do
    describe "with function declarations" $ do
      it "is True when function is declared with the given arity" $ do
        (declaresComputationWithExactArity 1) (named "f") (js "function f(x) { return x + 1 }") `shouldBe` True

      it "is False when function is declared with another arity" $ do
        (declaresComputationWithExactArity 2) (named "f") (js "function f(x) { x + 1}") `shouldBe` False

    describe "with constant declaration" $ do
      it "is True when constant is declared with lambda of given arity" $ do
        (declaresComputationWithExactArity 2) (named "f") (js "var f = function(x, y) { return x + y }") `shouldBe` True

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

  describe "isLongCode" $ do
    it "is False when the program has less than 16 nodes" $ do
      isLongCode (js "function f() { while(true) { console.log('foo') }  }")  `shouldBe` False

    it "is True when the program contains 16 or more nodes" $ do
      isLongCode (js "function f(){Poner(Verde) Mover(Norte) Poner(Verde)Mover(Norte) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde) Poner(Verde) Mover(Norte) Poner(Verde)Mover(Norte) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Este) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Sur) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde) Mover(Oeste) Poner(Verde)}")  `shouldBe` True

  describe "declaresObject" $ do
    it "is True when present" $ do
      declaresObject (named "f")  (js "var f = {x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresObject (named "f") (js "var f = 6")  `shouldBe` False

    it "is False when not present, scoped" $ do
      declaresObject (named "f") (js "var g = {}")  `shouldBe` False

    it "is True when present, scoped" $ do
      declaresObject (named "g") (js "var g = {}")  `shouldBe` True

    it "is True when anyone present, scoped" $ do
      declaresObject anyone (js "var g = {}")  `shouldBe` True

  describe "declaresMethod" $ do
    it "is True when present" $ do
      declaresMethod (named "x") (js "var f = {x: function(){}}")  `shouldBe` True

    it "is True when any present" $ do
      declaresMethod anyone (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when not present" $ do
      declaresMethod (named "m") (js "var f = {x: function(){}}")  `shouldBe` False

    it "is False when not a method" $ do
      declaresMethod (named "m") (js "var f = {x: 6}")  `shouldBe` False

    it "is True when object present, scoped" $ do
      scoped (declaresMethod (named "x")) "f"  (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when object not present, scoped" $ do
      scoped (declaresMethod (named "x")) "p"  (js "var f = {x: function(){}}")  `shouldBe` False

  describe "declaresAttribute" $ do
    it "is True when present" $ do
      declaresAttribute (named "x") (js "var f = {x: 6}")  `shouldBe` True

    it "is True when present and there are many" $ do
      declaresAttribute (named "x") (js "var f = {j: 20, x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresAttribute (named "m") (js "var f = {x: 6}")  `shouldBe` False

    it "is True when attribute present, scoped" $ do
      scoped (declaresAttribute (named "x")) "f"  (js "var f = {x: 6}")  `shouldBe` True

    it "is True when any attribute present, scoped" $ do
      scoped (declaresAttribute anyone) "f"  (js "var f = {x: 6}")  `shouldBe` True

    it "is False when attribute not present, scoped" $ do
      scoped (declaresAttribute (named "x")) "g" (js "var f = {x: 6}")  `shouldBe` False

  describe "uses" $ do
    it "is True on direct usage in function" $ do
      uses (named "m") (js "function f(x) { m }") `shouldBe` True

    it "is True on direct usage of something like it in function" $ do
      uses (like "m") (js "function f(x) { m2 }") `shouldBe` True

    it "is True on direct usage in method" $ do
      uses (named "m") (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is True on direct usage in method, scoped" $ do
      scoped (uses (named "m")) "o" (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped" $ do
      scoped (uses (named "p")) "o" (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is True on usage in method, scoped twice" $ do
      scopedList (uses (named "m")) ["o", "z"] (js "var o = {z: function(x) { m }}") `shouldBe` True

    it "is False on missing usage in method, scoped twice" $ do
      scopedList (uses (named "p")) ["o", "z"] (js "var o = {z: function(x) { m }}") `shouldBe` False

    it "is False on usage in wrong method, scoped twice" $ do
      scopedList (uses (named "m")) ["o", "z"] (js "var o = {p: function(x) { m }}") `shouldBe` False

    it "is True through function application in function" $ do
      transitive (uses (named "m")) "f" (js "function g() { m }; function f(x) { g() }") `shouldBe` True

    it "is True through function application in function" $ do
      transitive (uses (named "m")) "f" (js "function g(p) { return m }; function f(x) { return g(2) }") `shouldBe` True

    it "is False through function application in function" $ do
      transitive (uses (named "m")) "f" (js "function g() { m }; function f(x) { k() }") `shouldBe` False

    it "is True through message send in function" $ do
      transitive (uses (named "m")) "f" (js "var o = {g: function(){ m }}; function f(x) { o.g() }") `shouldBe` True

    it "is True through message send in objects" $ do
      transitive (uses (named "m")) "p" (js "var o = {g: function(){ m }}\n\
                                        \var p = {n: function() { o.g() }}") `shouldBe` True

