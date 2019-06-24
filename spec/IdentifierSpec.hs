module IdentifierSpec (spec) where

import           Test.Hspec

import           Language.Mulang.Identifier

spec :: Spec
spec = do
  describe "like" $ do
    it "matches exactly" $ do
      like "amount" "amount" `shouldBe` True
      like "amount" "count" `shouldBe` False

    it "matches ignoring case" $ do
      like "amount" "Amount" `shouldBe` True
      like "amount" "AMOUNT" `shouldBe` True
      like "Amount" "AMOUNT" `shouldBe` True

    it "matches prefix" $ do
      like "amount" "amountOfHouses" `shouldBe` True

    it "matches suffix" $ do
      like "amount" "totalamount" `shouldBe` True
      like "amount" "total_amount" `shouldBe` True
      like "amount" "totalAmount" `shouldBe` True

  describe "unlike" $ do
    it "matches exactly" $ do
      unlike "amount" "amount" `shouldBe` False
      unlike "amount" "count" `shouldBe` True

    it "matches ignoring case" $ do
      unlike "amount" "Amount" `shouldBe` False
      unlike "amount" "AMOUNT" `shouldBe` False
      unlike "Amount" "AMOUNT" `shouldBe` False

    it "matches prefix" $ do
      unlike "amount" "amountOfHouses" `shouldBe` False

    it "matches suffix" $ do
      unlike "amount" "totalamount" `shouldBe` False
      unlike "amount" "total_amount" `shouldBe` False
      unlike "amount" "totalAmount" `shouldBe` False

  describe "likeAnyOf" $ do
    it "matches exactly" $ do
      likeAnyOf ["amount", "count"] "amount" `shouldBe` True
      likeAnyOf ["amount", "count"] "count" `shouldBe` True
      likeAnyOf ["amount", "count"] "transaction" `shouldBe` False

    it "matches ignoring case" $ do
      likeAnyOf ["amount", "count"] "Amount" `shouldBe` True
      likeAnyOf ["amount", "count"] "AMOUNT" `shouldBe` True
      likeAnyOf ["Amount", "Count"] "COUNT" `shouldBe` True

    it "matches prefix" $ do
      likeAnyOf ["amount", "count"] "amountOfHouses" `shouldBe` True

    it "matches suffix" $ do
      likeAnyOf ["amount", "count"] "totalamount" `shouldBe` True
      likeAnyOf ["amount", "count"] "total_amount" `shouldBe` True
      likeAnyOf ["amount", "count"] "totalAmount" `shouldBe` True
      likeAnyOf ["amount", "count"] "account" `shouldBe` True

  describe "likeNoneOf" $ do
    it "matches exactly" $ do
      likeNoneOf ["amount", "count"] "amount" `shouldBe` False
      likeNoneOf ["amount", "count"] "count" `shouldBe` False
      likeNoneOf ["amount", "count"] "transaction" `shouldBe` True

    it "matches ignoring case" $ do
      likeNoneOf ["amount", "count"] "Amount" `shouldBe` False
      likeNoneOf ["amount", "count"] "AMOUNT" `shouldBe` False
      likeNoneOf ["Amount", "Count"] "COUNT" `shouldBe` False

    it "matches prefix" $ do
      likeNoneOf ["amount", "count"] "amountOfHouses" `shouldBe` False

    it "matches suffix" $ do
      likeNoneOf ["amount", "count"] "totalamount" `shouldBe` False
      likeNoneOf ["amount", "count"] "total_amount" `shouldBe` False
      likeNoneOf ["amount", "count"] "totalAmount" `shouldBe` False
      likeNoneOf ["amount", "count"] "account" `shouldBe` False
