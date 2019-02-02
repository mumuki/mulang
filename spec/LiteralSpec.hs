module LiteralSpec (spec) where

import           Test.Hspec
import           Language.Mulang

spec :: Spec
spec = do
  describe "matchValue (literalMatcher" $ do
    it "works with numbers" $ do

      matchValue (literalMatcher "4") (MuNumber 4) `shouldBe` True
      matchValue (literalMatcher "4") (MuNumber 4.0) `shouldBe` True
      matchValue (literalMatcher "4.5") (MuNumber 4.5) `shouldBe` True

      matchValue (literalMatcher "4") (MuNumber 5.0) `shouldBe` False
      matchValue (literalMatcher "\"4\"") (MuNumber 4.0) `shouldBe` False
      matchValue (literalMatcher "foo") (MuNumber 4.0) `shouldBe` False

    it "works with strings" $ do
      matchValue (literalMatcher "\"4\"") (MuString "4") `shouldBe` True
      matchValue (literalMatcher "\"hello\"") (MuString "hello") `shouldBe` True

    it "works with symbols" $ do
      matchValue (literalMatcher "#foo") (MuSymbol "foo") `shouldBe` True

      matchValue (literalMatcher "#foo") (MuString "foo") `shouldBe` False

    it "works with chars" $ do
      matchValue (literalMatcher "'4'") (MuChar '4') `shouldBe` True
      matchValue (literalMatcher "'a'") (MuChar 'a') `shouldBe` True

      matchValue (literalMatcher "'abc'") (MuChar 'a') `shouldBe` False

    it "works with bools" $ do
      matchValue (literalMatcher "True") (MuBool True) `shouldBe` True
      matchValue (literalMatcher "False") (MuBool False) `shouldBe` True

      matchValue (literalMatcher "True") (MuSymbol "True") `shouldBe` False

    it "works with nil" $ do
      matchValue (literalMatcher "Nil") MuNil `shouldBe` True
      matchValue (literalMatcher "Nil") (MuString "Nil") `shouldBe` False

