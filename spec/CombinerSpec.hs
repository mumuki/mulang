module CombinerSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Haskell

spec :: Spec
spec = do
  describe "detect" $ do
    it "can detect inspections" $ do
      let code = hs "x = if True then True else False\n\
                 \y = 2\n\
                 \z = if True then True else False"
      detect usesIf code `shouldBe` ["x", "z"]

  describe "negate" $ do
    it "is False when inspection is true" $ do
      negative usesIf (hs "f x = g x") `shouldBe` True

    it "is True when inspection is False" $ do
      negative (declares (named "f")) (hs "f x = g x") `shouldBe` False

  describe "transitive" $ do
    it "is True when inspection can be proven transitively to be True" $ do
      transitive usesIf "f" (hs "f x = g x\n\
                              \g x = if c x then 2 else 3") `shouldBe` True

    it "is True when inspection can be proven transitively to be True\
       \in several steps" $ do
      transitive usesIf "f" (hs "f x = g x\n\
                           \g x = m x\n\
                           \m x = if c x then 2 else 3") `shouldBe` True

    it "is True when inspection can be proven transitively to be True\
       \in recursive functions" $ do
      transitive usesIf "f" (hs "f x = g x\n\
                           \g x = m x\n\
                           \m x = if f x then 2 else 3") `shouldBe` True

    it "is False when inspection can be proven transitively to be False\
           \in recursive functions" $ do
          transitive usesIf "f" (hs "f x = g x\n\
                               \g x = m x\n\
                               \m x = f x") `shouldBe` False

    it "is False when inspection can not be proven transitively to be True" $ do
      transitive usesIf "f" (hs "f x = g x") `shouldBe` False

    it "is False when inspection can be proven transitively to be False" $ do
      transitive usesIf "f" (hs "f x = g x") `shouldBe` False
