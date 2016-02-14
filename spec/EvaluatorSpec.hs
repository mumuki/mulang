module EvaluatorSpec(spec) where

import           Language.Mulang.Cli.Evaluator
import           Language.Mulang.Cli.Code
import           Test.Hspec

spec = describe "Evaluator" $ do
  it "evaluates empty expectations" $ do
    evaluate (Input (Code Haskell "x = 2") []) `shouldBe` (Output [] [])

  it "detects smells" $ do
    evaluate (Input (Code Haskell "x = \\y -> f y") []) `shouldBe` (Output [] [(Expectation "x" "Not:HasRedundantLambda")])

  it "evaluates present expectations" $ do
    let yHasBinding = (Expectation "y" "HasBinding")
    let xHasBinding = (Expectation "x" "HasBinding")
    evaluate (Input (Code Haskell "x = 2") [yHasBinding, xHasBinding]) `shouldBe` (Output [
                                                                        ExpectationResult yHasBinding False,
                                                                        ExpectationResult xHasBinding True] [])
  it "works with plain mulang format" $ do
    let yHasBinding = (Expectation "y" "HasBinding")
    let code = Code Mulang "Program []"
    evaluate (Input code [yHasBinding]) `shouldBe` (Output [ExpectationResult yHasBinding False] [])