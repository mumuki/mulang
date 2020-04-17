module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseCustomExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang.Ast
import Language.Mulang.Analyzer.Analysis (customExpectationResult, QueryResult)
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseCustomExpectations :: Expression -> Maybe String -> [QueryResult]
analyseCustomExpectations ast = map (runExpectation ast) . fromMaybe [] . fmap parseExpectations

runExpectation :: Expression -> Expectation -> QueryResult
runExpectation ast (Expectation name q) = (q, customExpectationResult name (compileTopQuery q ast))

