module EvaluatorSpec(spec) where

import           Language.Mulang.Cli.Interpreter
import           Language.Mulang.Cli.Code
import           Test.Hspec

spec = describe "Evaluator" $ do
  it "evaluates empty expectations" $ do
    evaluate (Input (Code Haskell "x = 2") []) `shouldBe` (Output [] [])

  it "detects smells" $ do
    evaluate (Input (Code Haskell "x = \\y -> f y") []) `shouldBe` (Output [] [(Expectation ["x"] "HasRedundantParameter" Nothing True False)])

  it "evaluates present expectations" $ do
    let ydeclares = (Expectation [] "declares" (Just "y") False False)
    let xdeclares = (Expectation [] "declares" (Just "x") False False)
    evaluate (Input (Code Haskell "x = 2") [ydeclares, xdeclares]) `shouldBe` (Output [
                                                                        ExpectationResult ydeclares False,
                                                                        ExpectationResult xdeclares True] [])
  it "works with plain mulang format" $ do
    let ydeclares = (Expectation [] "declares" (Just "y") False False)
    let code = Code Mulang "MuNull"
    evaluate (Input code [ydeclares]) `shouldBe` (Output [ExpectationResult ydeclares False] [])

