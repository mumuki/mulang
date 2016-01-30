{-# LANGUAGE OverloadedStrings #-}

module JsSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "foo" $ do
    it "simple assignation" $ do
      parseHaskell "x = 1" `shouldBe` parseJavaScript "var x = 1"

    it "simple string assignment" $ do
      parseHaskell "x = \"hello\"" `shouldBe` parseJavaScript "var x = 'hello'"

    it "string parsing" $ do
      parseJavaScript "var x = \"hello\"" `shouldBe` parseJavaScript "var x = 'hello'"

    it "simple assignation 2" $ do
      parseJavaScript "x" `shouldBe` Just (Variable "x")

    it "simple function declaration" $ do
      parseHaskell "f x = 1" `shouldBe` parseJavaScript "function f(x) { return 1 }"

    it "multiple params function declaration" $ do
      parseHaskell "f x y = 1" `shouldBe` parseJavaScript "function f(x, y) { return 1 }"

    it "constant function declaration" $ do
      parseHaskell "f = \\x -> x + 1" `shouldBe` parseJavaScript "var f = function(x) { return x + 1 }"

    it "numeric top level expression" $ do
      parseJavaScript "8" `shouldBe` Just (MuNumber 8)

    it "list literal top level expression" $ do
      parseJavaScript "[8, 7]" `shouldBe` Just (MuList [MuNumber 8, MuNumber 7])

    it "compacts everything" $ do
      parseJavaScript "8; 9" `shouldBe` Just (Sequence [MuNumber 8, MuNumber 9])

    it "handles blocks as sequences" $ do
      js "8; 9" `shouldBe` js "{8; 9}"

    it "handles booleans" $ do
      js "true" `shouldBe` MuBool True

    it "handles lambdas" $ do
      js "(function(x, y) { 1 })" `shouldBe` (Lambda [VariablePattern "x", VariablePattern "y"] (MuNumber 1))

    it "handles application" $ do
      js "f(2)" `shouldBe` (Application (Variable "f") (MuNumber 2))

    it "handles application with multiple args" $ do
      js "f(2, 4)" `shouldBe` (Application (Variable "f") (MuNumber 4))

    it "handles message send" $ do
      js "o.f(2)" `shouldBe` (Send (Variable "o") (Variable "f") [(MuNumber 2)])

    it "handles ifs" $ do
      js "if(x) y else z" `shouldBe` If (Variable "x") (Variable "y") (Variable "z")

    it "handles partial ifs" $ do
      js "if(x) y" `shouldBe` If (Variable "x") (Variable "y") MuUnit

    it "handles ternarys as ifs" $ do
      js "x ? y : z " `shouldBe` js "if (x) { y } else { z }"

    it "handles whiles" $ do
      js "while (x) { y }" `shouldBe` While (Variable "x") (Variable "y")

    it "handles objects" $ do
      js "({x: 6})" `shouldBe` MuObject (VariableDeclaration "x" (MuNumber 6))

    it "handles empty objects" $ do
      js "({})" `shouldBe` MuObject MuUnit


