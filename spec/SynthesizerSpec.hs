module SynthesizerSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Analyzer.Synthesizer
import           Language.Mulang.Analyzer hiding (spec)

run = synthesizeInspection

spec :: Spec
spec = do
  describe "correct primitive usages" $ do
    it "corrects haskell and" $ do
      run Haskell "otherwise"  `shouldBe` (Just "UsesOtherwise")

    it "corrects haskell and" $ do
      run Haskell "and" `shouldBe` Nothing
      run Haskell "&&"  `shouldBe` (Just "UsesAnd")

    it "corrects haskell or" $ do
      run Haskell "or" `shouldBe` Nothing
      run Haskell "||" `shouldBe` (Just "UsesOr")

    it "corrects haskell not" $ do
      run Haskell "not" `shouldBe` (Just "UsesNegation")
      run Haskell "!"   `shouldBe`  Nothing

    it "corrects java and" $ do
      run Java    "and" `shouldBe` Nothing
      run Java    "&&"  `shouldBe` (Just "UsesAnd")

    it "corrects python and" $ do
      run Python  "and" `shouldBe` (Just "UsesAnd")
      run Python  "&&"  `shouldBe` Nothing

    it "corrects ruby and" $ do
      run Ruby    "and" `shouldBe` (Just "UsesAnd")
      run Ruby    "&&"  `shouldBe` (Just "UsesAnd")

  describe "corrects keyword usages" $ do
    it "corrects haskell type" $ do
      run Haskell "type" `shouldBe` (Just "DeclaresTypeAlias")

    it "corrects java if" $ do
      run Java    "if"     `shouldBe` (Just  "UsesIf")

    it "corrects java class" $ do
      run Java    "class"  `shouldBe` (Just "DeclaresClass")

    it "corrects java interface" $ do
      run Java    "interface"  `shouldBe` (Just "DeclaresInterface")

    it "corrects java for" $ do
      run Java    "for"  `shouldBe` (Just "UsesForLoop")

    it "corrects python def" $ do
      run Python  "def" `shouldBe` (Just "DeclaresComputation")

    it "corrects ruby class" $ do
      run Ruby    "class"  `shouldBe` (Just "DeclaresClass")

    it "corrects ruby include" $ do
      run Ruby    "include"  `shouldBe` (Just "Includes")

    it "corrects ruby def" $ do
      run Ruby  "def" `shouldBe` (Just "DeclaresComputation")
