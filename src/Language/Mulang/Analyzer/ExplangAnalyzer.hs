module Language.Mulang.Analyzer.ExplangAnalyzer (
  analyseExplang) where

import Data.Maybe (fromMaybe)

import Language.Mulang hiding (Test)
import Language.Mulang.Analyzer.Analysis (ExplangTestResult(..))
import Language.Mulang.Analyzer.ExplangExpectationsCompiler (compileExpectation)

import Language.Explang (parseTests, Test (..))

analyseExplang :: Expression -> Maybe String -> [ExplangTestResult]
analyseExplang ast  = fromMaybe [] . fmap (map (runTest ast) . parseTests)

runTest :: Expression -> Test -> ExplangTestResult
runTest ast (Test name e) = ExplangTestResult name (compileExpectation e ast)

