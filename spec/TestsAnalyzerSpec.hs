module TestsAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Language.Mulang.Interpreter.Runner (TestResult(..), TestStatus(..))
import           Test.Hspec

result rs = emptyCompletedAnalysisResult { testResults = rs }

run language sample test extra = analyse (testsAnalysis (fragment sample) (ExternalTests (fragment test) (fmap fragment extra) Nothing))
  where
    fragment content = CodeSample language content

spec = describe "TestsAnalyzer" $ do
  it "works with empty tests" $ do
    (run JavaScript "function f(x) { return x + 1 }" "" Nothing ) `shouldReturn` emptyCompletedAnalysisResult

  it "works without extra" $ do
    (run
      JavaScript
      "function f(x) { return x + 1 }"
      "it('f increments by one', function() { assert.equals(f(1), 2) })"
      Nothing) `shouldReturn` result [TestResult ["f increments by one"] Success]

  it "works with extra, passing result" $ do
    (run
      JavaScript
      "function f(x) { return x + 1 }"
      "it('f increments by one', function() { assert.equals(f(1), m) })"
      (Just "var m = 2")) `shouldReturn` result [TestResult ["f increments by one"] Success]

  it "works with extra, failing result" $ do
    (run
      JavaScript
      "function f(x) { return x + 1 }"
      "it('f increments by one', function() { assert.equals(f(1), m) })"
      (Just "var m = 3")) `shouldReturn` result [TestResult ["f increments by one"] (Failure "MuString \"Expected MuNumber 2.0 but got: MuNumber 3.0\"")]

