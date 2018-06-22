module AstsGenerationSpec (spec) where

  import           Language.Mulang.Ast
  import           Language.Mulang.Analyzer hiding (result, spec)
  import           Test.Hspec

  result asts = AnalysisCompleted [] [] [] asts

  run language content generationType = analyse (astsGenerationAnalysis (CodeSample language content) generationType)

  spec = describe "AstsGeneration" $ do
    it "can generate null asts" $ do
      (run Haskell "x = 1" NoAst) `shouldReturn` (result [])

    it "can generate null asts" $ do
      (run Haskell "x = 1 + 3" RootExpressionAst) `shouldReturn` (result [Variable "x" (Application (Reference "+") [MuNumber 1.0,MuNumber 3.0])])

    it "can generate all asts" $ do
      (run Haskell "x = 1 + 3" AllExpressionsAsts) `shouldReturn` (result [
                                                                    Variable "x" (Application (Reference "+") [MuNumber 1.0,MuNumber 3.0]),
                                                                    Application (Reference "+") [MuNumber 1.0,MuNumber 3.0],
                                                                    Reference "+",
                                                                    MuNumber 1.0,
                                                                    MuNumber 3.0 ])


