module ExpressivenessSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Generic.Expressiveness (isMisspelled, wordsOf)
import           Language.Mulang.Parsers.Haskell (hs)
import           Text.Dictionary (toDictionary)
import           Text.Inflections.Tokenizer (camelCase)

spec :: Spec
spec = do
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

  describe "wordsOf" $ do
    it "can tokenize camelCase words" $ do
      wordsOf camelCase (hs "todayIsAGreatDay = True") `shouldBe` ["today", "is", "a", "great", "day"]
