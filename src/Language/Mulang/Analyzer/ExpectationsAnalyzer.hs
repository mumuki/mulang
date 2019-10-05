module Language.Mulang.Analyzer.ExpectationsAnalyzer (
  analyseExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.Finding (Finding, mapFindings)
import Language.Mulang.Analyzer.Analysis (Expectation, ExpectationResult(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)

analyseExpectations :: Expression -> Maybe [Expectation] -> Finding [ExpectationResult]
analyseExpectations content = mapFindings (analyseExpectation content) . (fromMaybe [])

analyseExpectation :: Expression -> Expectation -> Finding ExpectationResult
analyseExpectation ast e = fmap (ExpectationResult e) (compileAndEval e)
  where compileAndEval e = fmap ($ ast) (compileExpectation e)

