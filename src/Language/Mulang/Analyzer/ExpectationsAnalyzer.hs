module Language.Mulang.Analyzer.ExpectationsAnalyzer (
  analyseExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (Expectation, ExpectationResult(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)

analyseExpectations :: Expression -> Maybe [Expectation] -> [ExpectationResult]
analyseExpectations content = map (analyseExpectation content) . (fromMaybe [])

analyseExpectation :: Expression -> Expectation -> ExpectationResult
analyseExpectation ast e = ExpectationResult e (compileAndEval e)
  where compileAndEval e = (compileExpectation e) ast

