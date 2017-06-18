module ExpressivenessSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Generic.Expressiveness
import           Language.Mulang.Parsers.Haskell (hs)
import           Text.Dictionary (toDictionary)
import           Text.Inflections.Tokenizer (camelCase)

spec :: Spec
spec = do
  describe "isWellWritten" $ do
    let english = toDictionary ["a","day","great","is","today"]
    let style   = camelCase

    it "is True when it is a single, well written token" $ do
      isWellWritten style english (hs "today = True") `shouldBe` True

    it "is True when all tokens are well-written" $ do
      isWellWritten style english (hs "todayIsAGreatDay = True") `shouldBe` True

    it "is True when it is a single, bad written token" $ do
      isWellWritten style english (hs "tuday = True") `shouldBe` False

    it "is False when there are typos" $ do
      isWellWritten style english (hs "tudayIsAGreatDay = True") `shouldBe` False
      isWellWritten style english (hs "todayIsAGraetDay = True") `shouldBe` False

  describe "wordsOf" $ do
    it "can tokenize camelCase words" $ do
      wordsOf camelCase (hs "todayIsAGreatDay = True") `shouldBe` ["today", "is", "a", "great", "day"]
