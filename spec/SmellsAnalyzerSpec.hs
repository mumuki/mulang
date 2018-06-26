module SmellsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Language.Mulang.Ast
import           Test.Hspec

result smellResults = AnalysisCompleted [] smellResults [] Nothing

runExcept language content smells = analyse (smellsAnalysis (CodeSample language content) allSmells { exclude = Just smells })
runOnly language content smells = analyse (smellsAnalysis (CodeSample language content) noSmells { include = Just smells })

spec = describe "SmellsAnalyzer" $ do
  describe "Using domain language and nested structures" $ do
    let runRuby sample = analyse (domainLanguageAnalysis (MulangSample sample Nothing) (DomainLanguage Nothing (Just RubyCase) (Just 3) Nothing))
    it "works with empty set" $ do
      (runRuby (Sequence [
        (Object "Foo_Bar" (Sequence [
          (SimpleMethod "y" [] None),
          (SimpleMethod "aB" [] None),
          (SimpleMethod "fooBar" [] None)])),
        (Object "Foo" None)])) `shouldReturn` (result [
                                                    Expectation "y" "HasTooShortIdentifiers",
                                                    Expectation "aB" "HasTooShortIdentifiers",
                                                    Expectation "Foo_Bar" "HasWrongCaseIdentifiers",
                                                    Expectation "aB" "HasWrongCaseIdentifiers",
                                                    Expectation "fooBar" "HasWrongCaseIdentifiers"])

  describe "Using exclusion" $ do
    it "works with empty set" $ do
      (runExcept Haskell "fun x = if x then True else False" []) `shouldReturn` (result [Expectation "fun" "HasRedundantIf"])

    describe "detect domain language violations" $ do
      it "detects identifier length violations" $ do
        (runExcept Haskell "f x = x" []) `shouldReturn` (result [Expectation "f" "HasTooShortIdentifiers"])

      it "detects case violations" $ do
        (runExcept Haskell "fixme_now x = x" []) `shouldReturn` (result [Expectation "fixme_now" "HasWrongCaseIdentifiers"])

    describe "works with non-empty set" $ do
      it "dont reports smell when excluded" $ do
        (runExcept JavaScript
                  "function f() { var x = 1; return x }"
                  ["HasRedundantLocalVariableReturn", "HasTooShortIdentifiers"]) `shouldReturn` (result [])

      it "reports smell when not excluded and present" $ do
        (runExcept JavaScript
                  "function foo() { var aVariable = 1; return aVariable }"
                  []) `shouldReturn` (result [Expectation "foo" "HasRedundantLocalVariableReturn"])

      it "dont reports smell when not excluded and not present" $ do
        (runExcept JavaScript
                  "function foo() { return 1; }"
                  ["HasRedundantLocalVariableReturn"]) `shouldReturn` (result [])

  describe "Using inclusion" $ do
    it "works with empty set" $ do
      (runOnly Haskell "f x = if x then True else False" []) `shouldReturn` (result [])

  describe "Using inclusion" $ do
    it "works with empty set" $ do
      (runExcept Python "def funcion():\n  if True:\n    pass\n  else:\n    return 1" []) `shouldReturn` (result [Expectation "funcion" "HasEmptyIfBranches"])
