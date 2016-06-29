module EvaluatorSpec(spec) where

import           Language.Mulang.Cli.Interpreter
import           Language.Mulang.Cli.Code
import           Language.Mulang.Cli.Compiler
import           Test.Hspec

spec = describe "Evaluator" $ do
  describe "Advanced expectations" $ do
    it "evaluates empty expectations" $ do
      evaluate (Input (Code Haskell "x = 2") []) `shouldBe` (Output [] [])

    it "detects smells" $ do
      evaluate (Input (Code Haskell "x = \\y -> f y") []) `shouldBe` (Output [] [(Advanced ["x"] "HasRedundantParameter" Anyone True False)])

    it "evaluates present named expectations" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let xdeclares = (Advanced [] "declares" (Named "x") False False)
      evaluate (Input (Code Haskell "x = 2") [ydeclares, xdeclares]) `shouldBe` (Output [
                                                                          ExpectationResult ydeclares False,
                                                                          ExpectationResult xdeclares True] [])

    it "evaluates present expectations" $ do
      let declaresF = (Advanced [] "declaresFunction" Anyone False False)
      let declaresT = (Advanced [] "declaresTypeAlias" Anyone False False)
      evaluate (Input (Code Haskell "f x = 2") [declaresF, declaresT]) `shouldBe` (Output [
                                                                          ExpectationResult declaresF True,
                                                                          ExpectationResult declaresT False] [])
    it "works with plain mulang format" $ do
      let ydeclares = (Advanced [] "declares" (Named "y") False False)
      let code = Code Mulang "MuNull"
      evaluate (Input code [ydeclares]) `shouldBe` (Output [ExpectationResult ydeclares False] [])

  describe "Basic expectations" $ do
    it "works with HasBinding" $ do
      let xdeclares = (Basic "x" "HasBinding")
      evaluate (Input (Code Haskell "x = 2") [xdeclares]) `shouldBe` (Output [ExpectationResult xdeclares True] [])