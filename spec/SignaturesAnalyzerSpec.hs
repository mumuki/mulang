module SignaturesAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result signatures = AnalysisCompleted [] [] signatures noDomainLanguageViolations

run language content style = analyse (signaturesAnalysis (CodeSample language content) style)

spec = describe "SignturesAnalyzer" $ do
  it "handles MulangStyle" $ do
    (run Haskell "f x = x + 1" MulangStyle ) `shouldBe` (result ["-- f(x)"])

  it "handles HaskellStyle" $ do
    (run Haskell "f x = x + 1" HaskellStyle ) `shouldBe` (result ["-- f x"])

  it "handles PrologStyle" $ do
    (run Prolog "f(X):-g(X)." PrologStyle ) `shouldBe` (result ["%% f/1"])

  it "handles UntypedCStyle" $ do
    (run Haskell "f x = x + 1" UntypedCStyle) `shouldBe` (result ["// f(x)"])

