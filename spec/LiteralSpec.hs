module LiteralSpec (spec) where

import           Test.Hspec
import           Language.Mulang

spec :: Spec
spec = do
  describe "literal inspections" $ do
    it "works with numbers" $ do

      isNumber 4 (MuNumber 4) `shouldBe` True
      isNumber 4 (MuNumber 4.0) `shouldBe` True
      isNumber 4.5 (MuNumber 4.5) `shouldBe` True
      isNumber 4 (MuNumber 5.0) `shouldBe` False

    it "works with strings" $ do
      isString "4" (MuString "4") `shouldBe` True
      isString "hello" (MuString "hello") `shouldBe` True

    it "works with symbols" $ do
      isSymbol "foo" (MuSymbol "foo") `shouldBe` True
      isSymbol "foo" (MuString "bar") `shouldBe` False

    it "works with chars" $ do
      isChar '4' (MuChar '4') `shouldBe` True
      isChar 'a' (MuChar 'a') `shouldBe` True
      isChar 'a' (MuChar 'b') `shouldBe` False

    it "works with bools" $ do
      isBool True (MuBool True) `shouldBe` True
      isBool False (MuBool False) `shouldBe` True
      isBool True (MuSymbol "True") `shouldBe` False

    it "works with nil" $ do
      isNil MuNil `shouldBe` True
      isNil (MuString "Nil") `shouldBe` False
