{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module JavaScriptSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = js . unpack

spec :: Spec
spec = do
  describe "foo" $ do
    it "simple assignation" $ do
      js "var x = 1" `shouldBe` hs "x = 1"

    it "simple assignation, with let" $ do
      js "let x = 1" `shouldBe` hs "x = 1"

    it "simple string assignment" $ do
      js "var x = 'hello'" `shouldBe` hs "x = \"hello\""

    it "string parsing" $ do
      js "var x = \"hello\"" `shouldBe` js "var x = 'hello'"

    it "simple assignation 2" $ do
      js "x" `shouldBe` Reference "x"

    it "simple function application in var declaration" $ do
      js "var x = m(1, 2)" `shouldBe` hs "x = m 1 2"

    it "differentiates procedures and functions" $ do
      (js "function f() { return 1 }" /= js "function f() { 1 }") `shouldBe` True

    it "handles lambdas likes haskell does" $ do
      js "var m = function(x) { return 1 }" `shouldBe` hs "m = \\x -> 1"

    it "handles arrow functions with explicit returns" $ do
      js "var m = (x) => { return 1 }" `shouldBe` js "var m = function(x) { return 1 }"

    it "handles arrow functions" $ do -- TODO this is not entirely true
      js "var m = (x) => { 1 }" `shouldBe` js "var m = function(x) { 1 }"

    it "simple function declaration" $ do
      js "function f(x) { return 1 }" `shouldBe` hs "f x = 1"

    it "simple procedure declaration" $ do
      js "function f(x) { console.log('fruit') }" `shouldBe` (
                SimpleProcedure
                    "f"
                    [VariablePattern "x"]
                    (Print (MuString "fruit")))

    it "multiple params function declaration" $ do
      js "function f(x, y) { return 1 }" `shouldBe` hs "f x y = 1"

    it "constant function declaration" $ do
      js "var f = function(x) { return x + 1 }" `shouldBe` hs "f = \\x -> x + 1"

    it "numeric top level expression" $ do
      js "8" `shouldBe` MuNumber 8

    it "assignment should be Assignment" $ do
      js "x = 8" `shouldBe` (Assignment "x" (MuNumber 8))

    it "update should be Assignment" $ do
      js "x += 8" `shouldBe` (Assignment "x" (Application (Reference "+") [Reference "x",MuNumber 8.0]))

    it "increment should be Assignment" $ do
      js "x++" `shouldBe` (Assignment "x" (Application (Reference "+") [Reference "x", MuNumber 1]))

    it "decrement should be Assignment" $ do
      js "x--" `shouldBe` (Assignment "x" (Application (Reference "-") [Reference "x", MuNumber 1]))

    it "sum should be parseable" $ do
      js "x + y" `shouldBe` (Application (Reference "+") [Reference "x",Reference "y"])

    it "list literal top level expression" $ do
      js "[8, 7]" `shouldBe` MuList [MuNumber 8, MuNumber 7]

    it "compacts everything" $ do
      js "8; 9" `shouldBe`  Sequence [MuNumber 8, MuNumber 9]

    it "handles blocks as sequences" $ do
      js "8; 9" `shouldBe` js "{8; 9}"

    it "handles booleans" $ do
      js "true" `shouldBe` MuTrue

    it "handles negation" $ do
      js "!true" `shouldBe` (Application (Primitive Negation) [MuTrue])

    it "handles boolean binary operations" $ do
      js "true || false " `shouldBe` (Application (Primitive Or) [MuTrue, MuFalse])

    it "handles lambdas" $ do
      js "(function(x, y) { 1 })" `shouldBe` (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1))

    it "handles application" $ do
      js "f(2)" `shouldBe` (Application (Reference "f") [MuNumber 2])

    it "handles application with multiple args" $ do
      js "f(2, 4)" `shouldBe` (Application (Reference "f") [MuNumber 2, MuNumber 4])

    it "handles message send" $ do
      js "o.f(2)" `shouldBe` (Send (Reference "o") (Reference "f") [(MuNumber 2)])

    it "handles ifs" $ do
      js "if(x) y else z" `shouldBe` If (Reference "x") (Reference "y") (Reference "z")

    it "handles partial ifs" $ do
      js "if(x) y" `shouldBe` If (Reference "x") (Reference "y") None

    it "handles ternarys as ifs" $ do
      js "x ? y : z " `shouldBe` js "if (x) { y } else { z }"

    it "handles whiles" $ do
      js "while (x) { y }" `shouldBe` While (Reference "x") (Reference "y")

    it "handles objects" $ do
      js "({x: 6})" `shouldBe` MuObject (Variable "x" (MuNumber 6))

    it "handles objects with numeric keys" $ do
      js "({3: 6})" `shouldBe` MuObject (Variable "3" (MuNumber 6))

    it "handles objects with string keys" $ do
      js "({\"asd\": 6})" `shouldBe` MuObject (Variable "asd" (MuNumber 6))

    it "handles empty objects" $ do
      js "({})" `shouldBe` MuObject None

    it "handles object declarations" $ do
      js "var x = {}" `shouldBe` (Object "x" None)

    it "handles function declarations as vars" $ do
      js "var x = function(){}" `shouldBe` (SimpleFunction "x" [] None)

    it "handles attribute and method declarations" $ do
      js "var x = {y: 2, z: function(){}}" `shouldBe` Object "x" (Sequence [
                                                            Attribute "y" (MuNumber 2.0),
                                                            SimpleMethod "z" [] None])

    it "handles new parentheses-less" $ do
      js "new Foo" `shouldBe` New (Reference "Foo") []

    it "handles new with parentheses" $ do
      js "new Foo()" `shouldBe` New (Reference "Foo") []

    it "handles new with args" $ do
      js "new Foo(1, 2)" `shouldBe` New (Reference "Foo") [MuNumber 1, MuNumber 2]

    it "handles new with args" $ do
      js "new Foo(1, 2)" `shouldBe` New (Reference "Foo") [MuNumber 1, MuNumber 2]

    it "handles switch" $ do
      run [text|
      switch(a){
        case 1: return 1;
        case 2: return 2;
      }|] `shouldBe` Switch (Reference "a") [(MuNumber 1, Return (MuNumber 1)), (MuNumber 2, Return (MuNumber 2))] (None)

    it "handles new with args" $ do
      run [text|
      switch(a){
        case 1: return 1;
        default: return 3;
      }|] `shouldBe` Switch (Reference "a") [(MuNumber 1, Return (MuNumber 1))] (Return (MuNumber 3))

    it "handles c-style for" $ do
      run "for(i = 0; i < 3; i++) i;" `shouldBe` ForLoop (Assignment "i" (MuNumber 0)) (js "i < 3") (js "i++") (Reference "i")

    it "handles c-style for with var" $ do
      run "for(var i = 0; i < 3; i++) i;" `shouldBe` ForLoop (Variable "i" (MuNumber 0)) (js "i < 3") (js "i++") (Reference "i")

    it "handles for in" $ do
      run "for(i in [1,2]) i;" `shouldBe` For [Generator (VariablePattern "i") (MuList [MuNumber 1, MuNumber 2])] (Reference "i")

    it "handles for var in" $ do
      run "for(var i in [1,2]) i;" `shouldBe` For [Generator (VariablePattern "i") (MuList [MuNumber 1, MuNumber 2])] (Reference "i")

    context "handles assertions" $ do
      it "handles truth assertions" $ do
        run "assert(true)" `shouldBe` Assert False (Truth $ MuBool True)

      it "handles equality assertions" $ do
        run "assert.equals(123, 321)" `shouldBe` Assert False (Equality (MuNumber 123) (MuNumber 321))

      it "handles equality assertions" $ do
        run "assert.notEquals(123, 321)" `shouldBe` Assert True (Equality (MuNumber 123) (MuNumber 321))

      it "handles failure assertions" $ do
        run "assert.throws(function() { throw('abc') }, 'abc')" `shouldBe` Assert False (Failure (Lambda [] (Raise (MuString "abc"))) (MuString "abc"))
