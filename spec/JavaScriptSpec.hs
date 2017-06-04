{-# LANGUAGE OverloadedStrings #-}

module JavaScriptSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Builder
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "foo" $ do
    it "simple assignation" $ do
      js "var x = 1" `shouldBe` hs "x = 1"

    it "simple string assignment" $ do
      js "var x = 'hello'" `shouldBe` hs "x = \"hello\""

    it "string parsing" $ do
      js "var x = \"hello\"" `shouldBe` js "var x = 'hello'"

    it "simple assignation 2" $ do
      js "x" `shouldBe` Reference "x"

    it "simple function application in var declaration" $ do
      hs "x = m 1 2" `shouldBe` js "var x = m(1, 2)"

    it "differentiates procedures and functions" $ do
      (js "function f() { return 1 }" /= js "function f() { 1 }") `shouldBe` True

    it "handles lambdas likes haskell does" $ do
      js "var m = function(x) { return 1 }" `shouldBe` hs "m = \\x -> 1"

    it "simple function declaration" $ do
      hs "f x = 1" `shouldBe` js "function f(x) { return 1 }"

    it "simple procedure declaration" $ do
      js "function f(x) { console.log('fruit') }" `shouldBe` (
                Procedure
                    "f"
                    [Equation [VariablePattern "x"] (UnguardedBody (Send (Reference "console") (Reference "log") [MuString "fruit"]))])

    it "multiple params function declaration" $ do
      hs "f x y = 1" `shouldBe` js "function f(x, y) { return 1 }"

    it "constant function declaration" $ do
      hs "f = \\x -> x + 1" `shouldBe` js "var f = function(x) { return x + 1 }"

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
      js "true" `shouldBe` MuBool True

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
      js "if(x) y" `shouldBe` If (Reference "x") (Reference "y") MuNull

    it "handles ternarys as ifs" $ do
      js "x ? y : z " `shouldBe` js "if (x) { y } else { z }"

    it "handles whiles" $ do
      js "while (x) { y }" `shouldBe` While (Reference "x") (Reference "y")

    it "handles objects" $ do
      js "({x: 6})" `shouldBe` MuObject (Variable "x" (MuNumber 6))

    it "handles empty objects" $ do
      js "({})" `shouldBe` MuObject MuNull

    it "handles object declarations" $ do
      js "var x = {}" `shouldBe` (Object "x" MuNull)

    it "handles function declarations as vars" $ do
      js "var x = function(){}" `shouldBe` (Function "x" (unguardedBody [] MuNull))

    it "handles attribute and method declarations" $ do
      js "var x = {y: 2, z: function(){}}" `shouldBe` (Object "x" (Sequence [
                                                              Attribute "y" (MuNumber 2.0),
                                                              Method "z" (unguardedBody [] MuNull)]))




