module TokenizerSpec (spec) where

import           Test.Hspec
import           Text.Inflections.Tokenizer (snakeCase, camelCase, tokenize, canTokenize)

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "can tokenize camelCase words" $ do
      tokenize camelCase "todayIsAGreatDay" `shouldBe` ["today", "is", "a", "great", "day"]

    it "can tokenize camelCase words with numbers" $ do
      tokenize camelCase "todayIsAGreatDay2" `shouldBe` ["today", "is", "a", "great", "day"]

    it "can tokenize snake_case words" $ do
      tokenize snakeCase "the_first_and_the_last" `shouldBe` ["the", "first", "and", "the", "last"]

    it "can tokenize snake_case words with numbers" $ do
      tokenize snakeCase "the_first_and_the_last_2" `shouldBe` ["the", "first", "and", "the", "last", "2"]

    it "can tokenize snake_case words with numbers using a wrong case" $ do
      tokenize camelCase "the_first_and_the_last_2" `shouldBe` []

    it "can tokenize snake_case words with numbers using a wrong case" $ do
      canTokenize camelCase "the_first_and_the_last_2" `shouldBe` False
