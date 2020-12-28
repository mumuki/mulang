module CombinerSpec (spec) where

import           Data.Function.Extra (never)

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Inspector.Generic.Smell
import           Language.Mulang.Parsers.Haskell
import           Language.Mulang.Parsers.JavaScript

spec :: Spec
spec = do
  describe "detect" $ do
    it "can detect inspections" $ do
      let code = hs "x = if True then True else False\n\
                    \y = 2\n\
                    \z = if True then True else False"
      detect usesIf code `shouldBe` ["x", "z"]

    it "can detect inspections at top level" $ do
      detect doesConsolePrint (js "console.log(hello)") `shouldBe` []

  describe "locate" $ do
    it "can locate nested inspections" $ do
      let code = hs "x = if True then True else False\n\
                    \y = 2\n\
                    \z = if True then True else False"
      locate usesIf code `shouldBe` Nested ["x", "z"]

    it "can locate inspections at top level" $ do
      locate doesConsolePrint (js "console.log(hello)") `shouldBe` TopLevel

    it "fails when inspection is not present" $ do
      locate usesWhile (js "hello") `shouldBe` Nowhere

  describe "never" $ do
    it "is False when inspection is true" $ do
      never usesIf (hs "f x = g x") `shouldBe` True

    it "is True when inspection is False" $ do
      never (declares (named "f")) (hs "f x = g x") `shouldBe` False

  describe "transitive" $ do
    it "is True when inspection can be proven transitively to be True" $ do
      transitive "f" usesIf (hs "f x = g x\n\
                              \g x = if c x then 2 else 3") `shouldBe` True

    it "is True when inspection can be proven transitively to be True\
       \in several steps" $ do
      transitive "f" usesIf (hs "f x = g x\n\
                           \g x = m x\n\
                           \m x = if c x then 2 else 3") `shouldBe` True

    it "is True when inspection can be proven transitively to be True\
       \in recursive functions" $ do
      transitive "f" usesIf (hs "f x = g x\n\
                           \g x = m x\n\
                           \m x = if f x then 2 else 3") `shouldBe` True

    it "is False when inspection can be proven transitively to be False\
           \in recursive functions" $ do
          transitive "f" usesIf (hs "f x = g x\n\
                               \g x = m x\n\
                               \m x = f x") `shouldBe` False

    it "is False when inspection can not be proven transitively to be True" $ do
      transitive "f" usesIf (hs "f x = g x") `shouldBe` False

    it "is False when inspection can be proven transitively to be False" $ do
      transitive "f" usesIf (hs "f x = g x") `shouldBe` False
