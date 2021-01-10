module TransformerSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Ast.Operator
import           Language.Mulang.Parsers.JavaScript (js)
import           Language.Mulang.Analyzer.Analysis hiding (spec)
import           Language.Mulang.Analyzer.Transformer
import           Language.Mulang.Transform.Normalizer

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "transformMany" $ do
    it "transform with two paths" $ do
      transformMany (Variable "x" (MuObject None)) [
          [RenameVariables],
          [Normalize (unnormalized { convertObjectVariableIntoObject = True })]
        ] `shouldBe` [Variable "mulang_var_n0" (MuObject None), Object "x" None]

  describe "transform" $ do
    it "transform empty list" $ do
      transform (Variable "x" (MuNumber 1)) [] `shouldBe` (Variable "x" (MuNumber 1))

    it "transform with rename" $ do
      transform (Variable "x" (MuNumber 1)) [RenameVariables] `shouldBe` (Variable "mulang_var_n0" (MuNumber 1))

    it "transform with replace" $ do
      let ast = (Sequence [
                    Variable "x" (MuNumber 5),
                    Variable "y" (MuNumber 5)])

      transform ast [Replace "IsVariable:x" None] `shouldBe` Sequence [None, Variable "y" (MuNumber 5)]
      transform ast [Replace "IsDeclaration:x" None] `shouldBe` Sequence [None, Variable "y" (MuNumber 5)]

      transform ast [Replace "IsVariable" None] `shouldBe` Sequence [None, None]

    it "transform with crop" $ do
      transform (Sequence [
                    Variable "x" (MuNumber 5),
                    Variable "y" (MuNumber 5)]) [Crop "IsVariable:x"] `shouldBe` (Variable "y" (MuNumber 5))

    it "transform with normalization" $ do
      transform (Variable "x" (MuObject None)) [
        RenameVariables,
        Normalize (unnormalized { convertObjectVariableIntoObject = True })] `shouldBe` (Object "mulang_var_n0" None)

    it "transform with aliasing" $ do
      transform (js "foo(xs)") [Aliase (Map.singleton "foo" Size)] `shouldBe` (js "xs.length")
      transform (js "xs.foo()") [Aliase (Map.singleton "foo" Size)] `shouldBe` (js "xs.length")
