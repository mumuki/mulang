module FunctionalInspectorSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Functional
import           Language.Mulang.Parsers.Haskell

spec :: Spec
spec = do
  describe "usesComposition" $ do
    describe "when constant assignment" $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "x = y . z") `shouldBe` True

      it "is True when composition is present inside lambda" $ do
        usesComposition (hs "x = \\m -> y . z") `shouldBe` True

      it "is True when composition is present inside application" $ do
        usesComposition (hs "x = f (g.h) x") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "x = 1") `shouldBe` False

    describe "when unguarded function" $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "f x = (g . f) x") `shouldBe` True

      it "is True when composition is present within if" $ do
        usesComposition (hs "f x = if True then (g . f) x else 5") `shouldBe` True

      it "is True when composition is present within list" $ do
        usesComposition (hs "f x = [(g.h x), m]") `shouldBe` True

      it "is True when composition is present within comprehension" $ do
        usesComposition (hs "f x = [ (g.h x) m | m <- [1..20]]") `shouldBe` True

      it "is True when composition is present within where" $ do
        usesComposition (hs "f x = m\n\
                           \      where m = (f.g) ") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "f x = g x") `shouldBe` False

    describe "when guarded function " $ do
      it "is True when composition is present on top level" $ do
        usesComposition (hs "f x | c x = g . f $ x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is True when composition is present on guard" $ do
        usesComposition (hs "f x | (c . g) x = g x\n\
                           \    | otherwise = 4") `shouldBe` True

      it "is False when composition not present" $ do
        usesComposition (hs "f x | c x = f x\n\
                           \    | otherwise = 4") `shouldBe` False

  describe "usesPatternMatching" $ do
    it "is True when there Pattern Matching on List" $ do
      usesPatternMatching (hs "foo [] = 0\nfoo (x:xs) = 1 + foo xs") `shouldBe` True

    it "is False when there not Pattern Matching" $ do
      usesPatternMatching (hs "foo x = 2") `shouldBe` False

    it "is True when there Pattern Matching on Maybe" $ do
      usesPatternMatching (hs "foo Nothing = 0\nfoo (Just x) = 1") `shouldBe` True

    it "is True when there there Pattern Matching on anonima variable" $ do
      usesPatternMatching (hs "baz _ = 5 + 8") `shouldBe` True


