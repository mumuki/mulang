module SmellsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Language.Mulang.Ast
import           Test.Hspec

result smells
  = emptyCompletedAnalysisResult { smells = smells }

runExcept language content smells = analyse (smellsAnalysis (CodeSample language content) (allSmellsBut smells))
runOnly language content smells = analyse (smellsAnalysis (CodeSample language content) (noSmellsBut smells))

spec = describe "SmellsAnalyzer" $ do
  it "Using domain language and nested structures" $ do
    let runRuby sample = analyse (domainLanguageAnalysis (MulangSample sample Nothing) (DomainLanguage Nothing (Just RubyCase) (Just 3) Nothing))
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

  it "works inferring domain language" $ do
    let runPython sample = runExcept Python3 sample []
    runPython "def fooBar():\n\tpass\n\ndef foo_baz():\n\tpass\n\n" `shouldReturn` (result [Expectation "fooBar" "HasWrongCaseIdentifiers"])

  it "works inferring caseStyl" $ do
    let runPython sample = analyse (domainLanguageAnalysis (CodeSample Python3 sample) (DomainLanguage Nothing Nothing (Just 3) Nothing))
    runPython "def fooBar():\n\tpass\n\ndef foo_baz():\n\tpass\n\n" `shouldReturn` (result [Expectation "fooBar" "HasWrongCaseIdentifiers"])

  describe "Using exclusion" $ do
    it "works with empty set, in java" $ do
      (runExcept Java "class Foo { static { System.out.println(\"foo\"); } }" []) `shouldReturn` (result [Expectation "Foo" "DoesConsolePrint"])

    it "works with empty set, in haskell" $ do
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

    describe "works with top level detections" $ do
      it "works with empty set, in Python" $ do
        (runExcept Python "print(4)" []) `shouldReturn` (result [Expectation "*" "DoesConsolePrint"])

      it "works with empty set, in JavaScript" $ do
        (runExcept JavaScript "console.log(4)" []) `shouldReturn` (result [Expectation "*" "DoesConsolePrint"])


  describe "Using inclusion" $ do
    it "works with empty set, in haskell" $ do
      (runOnly Haskell "f x = if x then True else False" []) `shouldReturn` (result [])

    it "works with empty set, in python" $ do
      (runExcept Python "def funcion():\n  if True:\n    pass\n  else:\n    return 1" []) `shouldReturn` (result [Expectation "funcion" "HasEmptyIfBranches"])
