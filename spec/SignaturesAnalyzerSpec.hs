module SignaturesAnalyzerSpec(spec) where

import           Language.Mulang.Analyzer hiding (result, spec)
import           Test.Hspec

result signatures
  = emptyCompletedAnalysisResult { signatures = signatures }

run language content style = analyse (signaturesAnalysis (CodeSample language content) style)

spec = describe "SignturesAnalyzer" $ do
  it "handles MulangStyle" $ do
    (run Haskell "f x = x + 1" MulangStyle ) `shouldReturn` (result ["-- f(x)"])
    (run Haskell "f :: Int -> String" MulangStyle ) `shouldReturn` (result ["-- f(Int): String"])

  it "handles HaskellStyle" $ do
    (run Haskell "f x = x + 1" HaskellStyle ) `shouldReturn` (result ["-- f x"])

  it "handles PrologStyle" $ do
    (run Prolog "f(X):-g(X)." PrologStyle ) `shouldReturn` (result ["%% f/1"])

  it "handles UntypedCStyle" $ do
    (run Haskell "f x = x + 1" UntypedCStyle) `shouldReturn` (result ["// f(x)"])

