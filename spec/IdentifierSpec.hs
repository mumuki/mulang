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
