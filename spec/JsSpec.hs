{-# LANGUAGE OverloadedStrings #-}

module JsSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Smell
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "foo" $ do
    it "simple assignation" $ do
      parseHaskell "x = 1" `shouldBe` parseJavaScript "var x = 1"

    it "simple assignation 2" $ do
      parseHaskell "x = 1" `shouldBe` parseJavaScript "x"



