module ExpressivenessSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Inspector.Expressiveness
import           Language.Mulang.Parsers.Haskell (hs)

spec :: Spec
spec = do
  describe "isWellWritten" $ do
    it "is True when it is a single, well written token" $ do
      isWellWritten english (hs "today = True") `shouldBe` True

    it "is True when all tokens are well-written" $ do
      isWellWritten english (hs "todayIsAGreatDay = True") `shouldBe` True

    it "is True when it is a single, bad written token" $ do
      isWellWritten english (hs "tuday = True") `shouldBe` False

    it "is False when there are typos" $ do
      isWellWritten english (hs "tudayIsAGreatDay = True") `shouldBe` False
      isWellWritten english (hs "todayIsAGraetDay = True") `shouldBe` False

  describe "wordsOf" $ do
    it "can tokenize camelCase words" $ do
      wordsOf (hs "todayIsAGreatDay = True") `shouldBe` ["today", "is", "a", "great", "day"]
