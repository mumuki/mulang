module Language.Mulang.Analyzer.ExpectationsAnalyzer (
  analyseExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang.Ast (Expression)
import Language.Mulang.Analyzer.Analysis (Expectation, QueryResult, ExpectationResult(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)
import Language.Mulang.Edl.Expectation (Query)

analyseExpectations :: Expression -> Maybe [Expectation] -> [QueryResult]
analyseExpectations content = map (analyseExpectation content) . (fromMaybe [])

analyseExpectation :: Expression -> Expectation -> QueryResult
analyseExpectation ast e = (query, ExpectationResult e (inspection ast))
  where (query, inspection) = compileExpectation e

