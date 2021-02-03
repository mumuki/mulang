module SmellsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Language.Mulang.Ast
import           Test.Hspec

result smells
  = emptyCompletedAnalysisResult { smells = smells }

runExcept language content smells = analyse (smellsAnalysis (CodeSample language content) (allSmellsBut smells))
runOnly language content smells = analyse (smellsAnalysis (CodeSample language content) (noSmellsBut smells))

runWithTypos language content expectations = do
  let spec = emptyAnalysisSpec { smellsSet = (allSmellsBut []), expectations = Just expectations }
  result <- analyse (Analysis (CodeSample language content) spec)
  return $ smells result

spec = describe "SmellsAnalyzer" $ do
  describe "Using language specific smells" $ do
    it "works with JavaScript smells" $ do
      (runOnly JavaScript "function f() { var x = 1 }" ["JavaScript#UsesVarInsteadOfLet"]) `shouldReturn` (result [Expectation "f" "JavaScript#UsesVarInsteadOfLet"])
      (runOnly JavaScript "function f() { let x = 1 }" ["JavaScript#UsesVarInsteadOfLet"]) `shouldReturn` (result [])

  describe "using usage typos" $ do
    it "works when there are missing usages and typos" $ do
      runWithTypos JavaScript "baz()" [Expectation "*" "Uses:bar"] `shouldReturn` [Expectation "baz" "HasUsageTypos:bar"]

    it "works when there are missing usages and multiple potential typos" $ do
      let typos = [ Expectation "baz" "HasUsageTypos:bar", Expectation "Bar" "HasUsageTypos:bar" ]

      runWithTypos JavaScript "baz()\nBar() {}" [Expectation "*" "Uses:bar"] `shouldReturn` typos

    it "works when there are missing usages but no typos" $ do
      runWithTypos JavaScript "goo()" [Expectation "*" "Uses:bar"] `shouldReturn` []

    it "works when there are no missing usages and potential typos" $ do
        runWithTypos JavaScript "bar()\nbaz()\n" [Expectation "*" "Uses:bar"] `shouldReturn` []

  describe "using declaration typos" $ do
    it "works when there are missing declarations and typos" $ do
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "Declares:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "DeclaresComputation:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "DeclaresComputationWithArity0:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "DeclaresComputationWithArity1:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "DeclaresProcedure:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "DeclaresFunction:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "Delegates:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "SubordinatesDeclarationsTo:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]

    it "works when there are duplicated expectations" $ do
      runWithTypos JavaScript "function baz() {}" [Expectation "*" "Declares:bar", Expectation "*" "Declares:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]

    it "works when there are duplicated expectation results" $ do
      runWithTypos Java "class Foo {  void baz() {} }" [Expectation "*" "Declares:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]

    it "works when there are similar declares expectations results" $ do
      runWithTypos Java "class Foo {  void baz() {} }" [Expectation "*" "Declares:bar", Expectation "*" "DeclaresComputationWithArity0:bar"] `shouldReturn` [Expectation "baz" "HasDeclarationTypos:bar"]

    it "works when there are missing declarations and multiple potential typos" $ do
      let typos = [ Expectation "Bar" "HasDeclarationTypos:bar", Expectation "baz" "HasDeclarationTypos:bar" ]

      runWithTypos JavaScript "function baz() {}\nfunction Bar() {}" [Expectation "*" "Declares:bar"] `shouldReturn` typos

    it "works when there are missing declarations but no typos" $ do
      runWithTypos JavaScript "function goo() {}" [Expectation "*" "Declares:bar"] `shouldReturn` []

    it "works when there are no missing declarations and potential typos" $ do
        runWithTypos JavaScript "function bar() {}\nfunction baz() {}\n" [Expectation "*" "Declares:bar"] `shouldReturn` []

    it "works when there are no missing declarations but original expectations fail" $ do
        runWithTypos JavaScript "function bar() {}" [Expectation "*" "DeclaresClass:bar"] `shouldReturn` []
        runWithTypos JavaScript "function bar() {}" [Expectation "*" "DeclaresComputationWithArity1:bar"] `shouldReturn` []
        runWithTypos JavaScript "function bar() {}" [Expectation "*" "Delegates:bar"] `shouldReturn` []
        runWithTypos JavaScript "function bar() {}\nfunction other(){}" [Expectation "*" "SubordinatesDeclarationsTo:bar"] `shouldReturn` []

  it "Using domain language and nested structures" $ do
    let runRuby sample = analyse (domainLanguageAnalysis (MulangSample (Just sample)) (DomainLanguage Nothing (Just RubyCase) (Just 3) Nothing))
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

  it "works inferring caseStyle" $ do
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
                  "function f() { let x = 1; return x }"
                  ["HasRedundantLocalVariableReturn", "HasTooShortIdentifiers"]) `shouldReturn` (result [])

      it "reports smell when not excluded and present" $ do
        (runExcept JavaScript
                  "function foo() { let aVariable = 1; return aVariable }"
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
      (runExcept Python "def funcion():\n  if True:\n    pass\n  else:\n    return 1" []) `shouldReturn` (result [Expectation "funcion" "ShouldInvertIfCondition"])
