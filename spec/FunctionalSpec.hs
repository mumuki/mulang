{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module FunctionalSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Inspector.Generic
import           Language.Mulang.Inspector.Functional

spec :: Spec
spec = do
  describe "usesGuards" $ do
    describe "detects guards when" $ do
      it "is present" $ do
        usesGuards (hs "f x | c x = 2\n\
                      \    | otherwise = 4") `shouldBe` True

      it "is present" $ do
        usesGuards (hs "f x = c x == 2") `shouldBe` False

  describe "lambda analyzer" $ do
    describe "detects lambdas when" $ do
      it "is present" $ do
        usesLambda (hs "f x = \\y -> 4") `shouldBe` True

      it "is present" $ do
        usesLambda (hs "f x = 4") `shouldBe` False


  describe "usesAnonymousVariable" $ do
    it "is True if _ is present in paramenters" $ do
      usesAnonymousVariable (hs "foo _ = 1") `shouldBe` True

    it "is True if _ is present in nested list patterns" $ do
      usesAnonymousVariable (hs "foo [3, _] = 1") `shouldBe` True

    it "is True if _ is present in nested infix application patterns" $ do
      usesAnonymousVariable (hs "foo (x:_) = 1") `shouldBe` True

    it "is True if _ is present in nested application patterns" $ do
      usesAnonymousVariable (hs "foo (F _ 1) = 1") `shouldBe` True

    it "is True if _ is present in nested tuple patterns" $ do
      usesAnonymousVariable (hs "foo (_, 1) = 1") `shouldBe` True

    it "is True if _ is present in nested at patterns" $ do
      usesAnonymousVariable (hs "foo x@(_, 1) = 1") `shouldBe` True

    it "is False if _ is not present in parameters" $ do
      usesAnonymousVariable (hs "foo x = 1") `shouldBe` False

    it "is False if _ is present only in seccond equation" $ do
      let code = hs . unlines $ ["foo False bool = bool", "foo True _ = True"]
      usesAnonymousVariable code `shouldBe` True

    it "is False if there is no _ but a comment" $ do
      usesAnonymousVariable (hs "foo x = 1\n--") `shouldBe` False

    it "is False if there is only a comment" $ do
      usesAnonymousVariable (hs "--") `shouldBe` False

  describe "usesForComprehension" $ do
    it "is True when list comprehension exists" $ do
      usesForComprehension (hs "x = [m|m<-t]") `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      usesForComprehension (hs "x = []") `shouldBe` False

    it "is True when do syntax is used" $ do
      usesForComprehension (hs "y = do { x <- xs; return x }") `shouldBe` True

  describe "usesComprehension" $ do
    it "is True when list comprehension exists" $ do
      usesComprehension (hs "x = [m|m<-t]") `shouldBe` True

    it "is False when comprehension doesnt exists" $ do
      usesComprehension (hs "x = []") `shouldBe` False
