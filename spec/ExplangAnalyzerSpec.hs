module ExplangAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result explangTestResults smells
  = emptyCompletedAnalysisResult { explangTestResults = explangTestResults, smells = smells }

run language content test = analyse (explangTestAnalysis (CodeSample language content) test)

passed message = ExplangTestResult message True
failed message = ExplangTestResult message False

spec = describe "ExpectationsAnalyzer" $ do
  it "evaluates usesIf" $ do
    (run JavaScript "" "test \"must use if\": UsesIf") `shouldReturn` (result [failed "must use if"] [])
    (run JavaScript "if (true) {}" "test \"must use if\": UsesIf") `shouldReturn` (result [passed "must use if"] [])

  it "evaluates count(usesIf)" $ do
    (run JavaScript "" "test: count (UsesIf) = 0") `shouldReturn` (result [passed "E0"] [])
    (run JavaScript "" "test: count (UsesIf) = 1") `shouldReturn` (result [failed "E0"] [])

    (run JavaScript "" "test: count (UsesIf) >= 0") `shouldReturn` (result [passed "E0"] [])
    (run JavaScript "" "test: count (UsesIf) >= 1") `shouldReturn` (result [failed "E0"] [])
    (run JavaScript "" "test: count (UsesIf) >= 2") `shouldReturn` (result [failed "E0"] [])

    (run JavaScript "if (true) {}" "test: count (UsesIf) >= 0") `shouldReturn` (result [passed "E0"] [])
    (run JavaScript "if (true) {}" "test: count (UsesIf) >= 1") `shouldReturn` (result [passed "E0"] [])
    (run JavaScript "if (true) {}" "test: count (UsesIf) >= 2") `shouldReturn` (result [failed "E0"] [])

    (run JavaScript "if (true) {}; if (false) {}" "test: count (UsesIf) >= 0") `shouldReturn` (result [passed "E0"] [])
    (run JavaScript "if (true) {}; if (false) {}" "test: count (UsesIf) >= 1") `shouldReturn` (result [passed "E0"] [])
    (run JavaScript "if (true) {}; if (false) {}" "test: count (UsesIf) >= 2") `shouldReturn` (result [passed "E0"] [])

    (run JavaScript "if (true) {}; if(true) {}; if (false) {}" "test: count (UsesIf) >= 3") `shouldReturn` (result [passed "E0"] [])

