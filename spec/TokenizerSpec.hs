module TokenizerSpec (spec) where

import           Test.Hspec
import           Text.Inflections.Tokenizer (snakeCase, camelCase, tokenize)

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "can tokenize camelCase words" $ do
      tokenize camelCase "todayIsAGreatDay" `shouldBe` ["today", "is", "a", "great", "day"]

    it "can tokenize snake_case words" $ do
      tokenize snakeCase "the_first_and_the_last" `shouldBe` ["the", "first", "and", "the", "last"]
