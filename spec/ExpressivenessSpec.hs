module ExpressivenessSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Generic.Expressiveness (isMisspelled, isTooShortBinding)
import           Language.Mulang.Parsers.Haskell (hs)
import           Text.Dictionary (toDictionary)
import           Text.Inflections.Tokenizer (camelCase)

spec :: Spec
spec = do
  describe "isTooShortBinding" $ do
    it "is True when it is a one-char binding" $ do
      isTooShortBinding (hs "x = True") `shouldBe` True

    it "is True when it is a two-chars binding" $ do
      isTooShortBinding (hs "xy = True") `shouldBe` True

    it "is False when it is a three-chars binding" $ do
      isTooShortBinding (hs "zip = []") `shouldBe` False

  describe "isMisspelled" $ do
    let english = toDictionary ["a","day","great","is","today"]
    let style   = camelCase

    it "is True when it is a single, well written token" $ do
      isMisspelled style english (hs "today = True") `shouldBe` False

    it "is True when all tokens are well-written" $ do
      isMisspelled style english (hs "todayIsAGreatDay = True") `shouldBe` False

    it "is True when it is a single, bad written token" $ do
      isMisspelled style english (hs "tuday = True") `shouldBe` True

    it "is False when there are typos" $ do
      isMisspelled style english (hs "tudayIsAGreatDay = True") `shouldBe` True
      isMisspelled style english (hs "todayIsAGraetDay = True") `shouldBe` True
