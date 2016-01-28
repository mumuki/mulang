{-# LANGUAGE OverloadedStrings #-}

module JsSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Inspector.Smell
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
      parseHaskell "x = 1" `shouldBe` parseJavaScript "x"

    it "simple function declaration" $ do
      parseHaskell "f x = 1" `shouldBe` parseJavaScript "function f(x) { return 1 }"

    it "constant function declaration" $ do
      parseHaskell "f = \\x -> x + 1" `shouldBe` parseJavaScript "var f = function(x) { return x + 1 }"

    it "numeric top level expression" $ do
      parseJavaScript "8" `shouldBe` Just (Sequence [Literal (MuInteger 8)])

    it "list literal top level expression" $ do
      parseJavaScript "[8, 7]" `shouldBe` Just (Sequence [MuList [Literal (MuInteger 8), Literal (MuInteger 7)]])

    it "compacts everything" $ do
      parseJavaScript "8; 9" `shouldBe` Just (Sequence [Literal (MuInteger 8)])

    it "handles blocks as sequences" $ do
      parseJavaScript "8; 9" `shouldBe` parseJavaScript "{8; 9}"



