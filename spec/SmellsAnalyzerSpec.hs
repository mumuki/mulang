module SmellsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result smellResults = AnalysisCompleted [] smellResults []

runExcept language content smells = analyse (smellsAnalysis (CodeSample language content) allSmells { exclude = smells })
runOnly language content smells = analyse (smellsAnalysis (CodeSample language content) onlySmells { include = smells })

spec = describe "SmellsAnalyzer" $ do
  describe "Using exclusion" $ do
    it "works with empty set" $ do
      (runExcept Haskell "fun x = if x then True else False" []) `shouldReturn` (result [Basic "fun" "HasRedundantIf"])

    describe "detect domain language violations" $ do
      it "detects binding length violations" $ do
        (runExcept Haskell "f x = x" []) `shouldReturn` (result [Basic "f" "HasTooShortBindings"])

      it "detects case violations" $ do
        (runExcept Haskell "fixme_now x = x" []) `shouldReturn` (result [Basic "fixme_now" "HasWrongCaseBindings"])

    describe "works with non-empty set" $ do
      it "dont reports smell when excluded" $ do
        (runExcept JavaScript
                  "function f() { var x = 1; return x }"
                  [HasRedundantLocalVariableReturn, HasTooShortBindings]) `shouldReturn` (result [])

      it "reports smell when not excluded and present" $ do
        (runExcept JavaScript
                  "function foo() { var x = 1; return x }"
                  []) `shouldReturn` (result [Basic "foo" "HasRedundantLocalVariableReturn"])

      it "dont reports smell when not excluded and not present" $ do
        (runExcept JavaScript
                  "function foo() { return 1; }"
                  [HasRedundantLocalVariableReturn]) `shouldReturn` (result [])

  describe "Using inclusion" $ do
    it "works with empty set" $ do
      (runOnly Haskell "f x = if x then True else False" []) `shouldReturn` (result [])
