module TransformerSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.Transformer
import           Language.Mulang.Transform.Normalizer

spec :: Spec
spec = do
  describe "transformMany" $ do
    it "transform with two paths" $ do
      transformMany (Variable "x" (MuObject None)) [
          [RenameVariables],
          [Normalize (defaultNormalizationOptions { convertObjectVariableIntoObject = True })]
        ] `shouldBe` [Variable "mulang_var_n0" (MuObject None), Object "x" None]

  describe "transform" $ do
    it "transform empty list" $ do
      transform (Variable "x" (MuNumber 1)) [] `shouldBe` (Variable "x" (MuNumber 1))

    it "transform with rename" $ do
      transform (Variable "x" (MuNumber 1)) [RenameVariables] `shouldBe` (Variable "mulang_var_n0" (MuNumber 1))

    it "transform with normalization" $ do
      transform (Variable "x" (MuObject None)) [
        RenameVariables,
        Normalize (defaultNormalizationOptions { convertObjectVariableIntoObject = True })] `shouldBe` (
          Object "mulang_var_n0" None)
