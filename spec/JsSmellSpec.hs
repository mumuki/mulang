{-# LANGUAGE OverloadedStrings #-}

module JsSmellSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "JS.hasRedundantLambda" $ do
    it "is True whn η-conversion applies" $ do
      hasRedundantLambda "x" (js "var x = function(m) { return f(m) }") `shouldBe` True

    it "is False when it is an unavoidable lambda" $ do
      hasRedundantLambda "x" (js "var x = function(m) { return m(f) }") `shouldBe` False

  describe "hasRedundantIf" $ do
    it "is True when if present and both branches are boolean literals" $ do
      hasRedundantIf "x" (js "function x() { if(m) true else false }") `shouldBe` True
      hasRedundantIf "x" (js "function x() { if(m) false else true }") `shouldBe` True

    it "is False when there is no if" $ do
      hasRedundantIf "x" (js "var x = false") `shouldBe` False

    it "is False when there are no literals" $ do
      hasRedundantIf "x" (js "function x() { if(m) 2 else 4 }") `shouldBe` False

  describe "hasRedundantBooleanComparison" $ do
    it "is True when comparing a literal in an if" $ do
      hasRedundantBooleanComparison "x" (js "function x(m) { return m == true }") `shouldBe` True

    it "is False when no comparison" $ do
      hasRedundantBooleanComparison "x" (js "function x(m) { return m }") `shouldBe` False
