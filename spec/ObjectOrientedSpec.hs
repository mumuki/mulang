{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module ObjectOrientedSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Parsers.JavaScript
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Parsers.Python (py2, py3)
import           Language.Mulang.Ast
import           Language.Mulang.Identifier
import           Language.Mulang.Inspector.ObjectOriented
import           Language.Mulang.Inspector.Combiner

spec :: Spec
spec = do
  describe "declaresInterface" $ do
    it "is True when present" $ do
      declaresInterface (named "Optional") (java "interface Optional { Object get(); }") `shouldBe` True

    it "is False when not present" $ do
      declaresInterface (named "Bird") (java "class Bird extends Animal {}") `shouldBe` False

  describe "instantiates" $ do
    it "is True when instantiates" $ do
      instantiates (named "Bird") (java "class Main {  void main(String[] args) { Animal a = new Bird(); }  }") `shouldBe` True

    it "is False when not instantiates" $ do
      instantiates (named "Bird") (java "class Main {  void main(String[] args) { Animal a = new Mammal(); }  }") `shouldBe` False

  describe "implements" $ do
    it "is True when implements" $ do
      implements (named "Bird") (java "class Eagle implements Bird {}") `shouldBe` True

    it "is False when implements declaration not present" $ do
      implements (named "Bird") (java "class Cell {}") `shouldBe` False

    it "is False when a superinterface is declares" $ do
      implements (named "Iterable") (java "interface Collection extends Iterable {}") `shouldBe` False

  describe "usesInheritance" $ do
    it "is True when present" $ do
      usesInheritance (Class "Bird" (Just "Animal") None) `shouldBe` True

    it "is False when not present" $ do
      usesInheritance (Class "Bird" Nothing None) `shouldBe` False

    it "is True when present, scoped" $ do
      (scoped "Bird" usesInheritance) (Sequence [Class "Bird" (Just "Animal") None, Class "Fox" (Just "Animal") None])  `shouldBe` True

    it "is True when present, scoped" $ do
      (scoped "Hercules" usesInheritance) (Sequence [Class "Hercules" Nothing None, Class "Fox" (Just "Animal") None])  `shouldBe` False

  describe "usesMixins" $ do
    it "is True when include present" $ do
      usesMixins (Class "Dragon" Nothing (Include (Reference "FlyingCreature"))) `shouldBe` True

    it "is False when include not present" $ do
      usesMixins (Class "Dragon" Nothing (Implement (Reference "FlyingCreature"))) `shouldBe` False

  describe "declaresMethod" $ do
    it "is True when present" $ do
      declaresMethod (named "x") (js "var f = {x: function(){}}")  `shouldBe` True

    it "is works with except" $ do
      declaresMethod (except "x") (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (except "a") (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (except "y") (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (except "b") (js "var obj = {b: function(){}}")  `shouldBe` False

    it "is works with anyOf" $ do
      declaresMethod (anyOf ["x", "y"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` False
      declaresMethod (anyOf ["a", "y"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (anyOf ["x", "b"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (anyOf ["a", "b"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True

    it "is works with noneOf" $ do
      declaresMethod (noneOf ["x", "y"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (noneOf ["x", "b"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (noneOf ["a", "y"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` True
      declaresMethod (noneOf ["a", "b"]) (js "var obj = {a: function(){}, b: function(){}}")  `shouldBe` False

    it "is True when any present" $ do
      declaresMethod anyone (js "var f = {x: function(){}}")  `shouldBe` True

    it "is True when scoped in a class" $ do
      scoped "A" (declaresMethod (named "foo")) (java "class A { void foo() {} }")  `shouldBe` True

    it "is False when scoped in a class and not present" $ do
      scoped "A" (declaresMethod (named "foo")) (java "class A { void foobar() {} }") `shouldBe` False

    it "is False when not present" $ do
      declaresMethod (named "m") (js "var f = {x: function(){}}")  `shouldBe` False

    it "is False when not a method" $ do
      declaresMethod (named "m") (js "var f = {x: 6}")  `shouldBe` False

    it "is True when object present, scoped" $ do
      scoped "f" (declaresMethod (named "x")) (js "var f = {x: function(){}}")  `shouldBe` True

    it "is False when object not present, scoped" $ do
      scoped "p" (declaresMethod (named "x")) (js "var f = {x: function(){}}")  `shouldBe` False

  describe "declaresAttribute" $ do
    it "is True when present" $ do
      declaresAttribute (named "x") (js "var f = {x: 6}")  `shouldBe` True

    it "is True when present and there are many" $ do
      declaresAttribute (named "x") (js "var f = {j: 20, x: 6}")  `shouldBe` True

    it "is False when not present" $ do
      declaresAttribute (named "m") (js "var f = {x: 6}")  `shouldBe` False

    it "is True when attribute present, scoped" $ do
      scoped "f" (declaresAttribute (named "x"))  (js "var f = {x: 6}")  `shouldBe` True

    it "is True when any attribute present, scoped" $ do
      scoped "f" (declaresAttribute anyone) (js "var f = {x: 6}")  `shouldBe` True

    it "is False when attribute not present, scoped" $ do
      scoped "g" (declaresAttribute (named "x")) (js "var f = {x: 6}")  `shouldBe` False

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

  describe "declaresEnumeration" $ do
    it "is True when present" $ do
      declaresEnumeration (named "Direction") (Enumeration "Direction" ["SOUTH", "EAST", "WEST", "NORTH"]) `shouldBe` True

    it "is False when not present" $ do
      declaresEnumeration (named "Bird") (Class "Bird" (Just "Animal") None) `shouldBe` False
