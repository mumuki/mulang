module CountSpec (spec) where

import           Test.Hspec

import           Data.Count

spec :: Spec
spec = do
  describe "encode" $ do
    it "encodes zero" $ do
      encode (0 :: Int) `shouldBe` Zero

    it "encodes one" $ do
      encode (1 :: Int) `shouldBe` One

    it "encodes two" $ do
      encode (2 :: Int) `shouldBe` MoreThanOne 2

    it "encodes three" $ do
      encode (3 :: Int) `shouldBe` MoreThanOne 3

    it "encodes 100" $ do
      encode (100 :: Int) `shouldBe` MoreThanOne 100

  describe "encodeMaybe" $ do
    it "encodes just" $ do
      encodeMaybe (0 :: Int) `shouldBe` Just Zero
      encodeMaybe (1 :: Int) `shouldBe` Just One
      encodeMaybe (2 :: Int) `shouldBe` Just (MoreThanOne 2)
      encodeMaybe (3 :: Int) `shouldBe` Just (MoreThanOne 3)
      encodeMaybe (100 :: Int) `shouldBe` Just (MoreThanOne 100)

    it "encodes nothing" $ do
      encodeMaybe ((-2) :: Int) `shouldBe` Nothing
      encodeMaybe ((-20) :: Int) `shouldBe` Nothing

  describe "cast" $ do
    it "casts positives" $ do
      cast (0 :: Int) `shouldBe` Zero
      cast (1 :: Int) `shouldBe` One
      cast (2 :: Int) `shouldBe` MoreThanOne 2

    it "casts negatives" $ do
      cast ((-2) :: Int) `shouldBe` Zero
      cast ((-20) :: Int) `shouldBe` Zero

  describe "Num" $ do
    it "is an integral literal" $ do
      0 `shouldBe` Zero
      1 `shouldBe` One
      10 `shouldBe` MoreThanOne 10
      100 `shouldBe` MoreThanOne 100

    it "+" $ do
      0 + 0 `shouldBe` Zero
      1 + 0 `shouldBe` One
      0 + 1 `shouldBe` One
      10 + 22 `shouldBe` MoreThanOne 32

    it "*" $ do
      0 * 0 `shouldBe` Zero
      1 * 0 `shouldBe` Zero
      2 * 3 `shouldBe` MoreThanOne 6
      10 * 30 `shouldBe` MoreThanOne 300

  describe "count" $ do
    it "counts finite lists" $ do
      count [] == 0 `shouldBe` True
      count [True] == 1 `shouldBe` True
      count [True, True, True, True] == 4 `shouldBe` True

    it "counts inifinte lists" $ do
      count ([1..] :: [Int]) == 0 `shouldBe` False
      count ([1..] :: [Int]) == 1 `shouldBe` False
      count ([1..] :: [Int]) >= 1 `shouldBe` True
      count ([1..] :: [Int]) < 1 `shouldBe` False

