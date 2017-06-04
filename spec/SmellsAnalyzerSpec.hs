module SmellsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result smellResults = AnalysisCompleted [] smellResults []

runExcept language content smells = analyse (smellsAnalysis (CodeSample language content) allSmells { exclude = smells })
runOnly language content smells = analyse (smellsAnalysis (CodeSample language content) noSmells { include = smells })

spec = describe "SmellsAnalyzer" $ do
  describe "Using exclusion" $ do
    it "works with empty set" $ do
      (runExcept Haskell "f x = if x then True else False" []) `shouldBe` (result [Basic "f" "HasRedundantIf"])

    describe "works with non-empty set hasRedundantLocalVariableReturn" $ do
      it "dont reports smell when excluded" $ do
        (runExcept JavaScript
                  "function foo() { var x = 1; return x }"
                  [HasRedundantLocalVariableReturn]) `shouldBe` (result [])

      it "reports smell when not excluded and present" $ do
        (runExcept JavaScript
                  "function foo() { var x = 1; return x }"
                  []) `shouldBe` (result [Basic "foo" "HasRedundantLocalVariableReturn"])

      it "dont reports smell when not excluded and not present" $ do
        (runExcept JavaScript
                  "function foo() { return 1; }"
                  [HasRedundantLocalVariableReturn]) `shouldBe` (result [])

  describe "Using inclusion" $ do
    it "works with empty set" $ do
      (runOnly Haskell "f x = if x then True else False" []) `shouldBe` (result [])
