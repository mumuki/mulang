module SmellsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result expectationsResults smellResults = AnalysisCompleted expectationsResults smellResults []

run language content = analyse (smellsAnalysis (CodeSample language content) allSmells)

spec = describe "SmellsAnalyzer" $ do
  it "detects HasRedundantIf" $ do
    (run Haskell "f x = if x then True else False") `shouldBe` (result [] [Basic "f" "HasRedundantIf"])

  it "detects hasRedundantLocalVariableReturn" $ do
    (run JavaScript "function foo() { var x = 1; return x }") `shouldBe` (result [] [Basic "foo" "HasRedundantLocalVariableReturn"])

