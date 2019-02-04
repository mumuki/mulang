module LiteralSpec (spec) where

import           Test.Hspec
import           Language.Mulang

spec :: Spec
spec = do
  describe "isLiteral" $ do
    it "works with numbers" $ do

      isLiteral "4" (MuNumber 4) `shouldBe` True
      isLiteral "4" (MuNumber 4.0) `shouldBe` True
      isLiteral "4.5" (MuNumber 4.5) `shouldBe` True

      isLiteral "4" (MuNumber 5.0) `shouldBe` False
      isLiteral "\"4\"" (MuNumber 4.0) `shouldBe` False
      isLiteral "foo" (MuNumber 4.0) `shouldBe` False

    it "works with strings" $ do
      isLiteral "\"4\"" (MuString "4") `shouldBe` True
      isLiteral "\"hello\"" (MuString "hello") `shouldBe` True

    it "works with symbols" $ do
      isLiteral "#foo" (MuSymbol "foo") `shouldBe` True

      isLiteral "#foo" (MuString "foo") `shouldBe` False

    it "works with chars" $ do
      isLiteral "'4'" (MuChar '4') `shouldBe` True
      isLiteral "'a'" (MuChar 'a') `shouldBe` True

      isLiteral "'abc'" (MuChar 'a') `shouldBe` False

    it "works with bools" $ do
      isLiteral "True" (MuBool True) `shouldBe` True
      isLiteral "False" (MuBool False) `shouldBe` True

      isLiteral "True" (MuSymbol "True") `shouldBe` False

    it "works with nil" $ do
      isLiteral "Nil" MuNil `shouldBe` True
      isLiteral "Nil" (MuString "Nil") `shouldBe` False
