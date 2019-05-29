module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseCustomExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang hiding (Expectation)
import Language.Mulang.Analyzer.Analysis (CustomExpectationResult(..))
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseCustomExpectations :: Expression -> Maybe String -> [CustomExpectationResult]
analyseCustomExpectations ast  = fromMaybe [] . fmap (map (runExpectation ast) . parseExpectations)

runExpectation :: Expression -> Expectation -> CustomExpectationResult
runExpectation ast (Expectation name q) = CustomExpectationResult name (compileTopQuery q ast)

