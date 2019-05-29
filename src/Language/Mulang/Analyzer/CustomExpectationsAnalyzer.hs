module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseExplang) where

import Data.Maybe (fromMaybe)

import Language.Mulang hiding (Expectation)
import Language.Mulang.Analyzer.Analysis (ExplangTestResult(..))
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseExplang :: Expression -> Maybe String -> [ExplangTestResult]
analyseExplang ast  = fromMaybe [] . fmap (map (runExpectation ast) . parseExpectations)

runExpectation :: Expression -> Expectation -> ExplangTestResult
runExpectation ast (Expectation name q) = ExplangTestResult name (compileTopQuery q ast)

