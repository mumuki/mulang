module Language.Mulang.Analyzer.ExpectationsAnalyzer (
  analyseExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.ExpectationsEvaluator (ExpectationsEvaluator)
import Language.Mulang.Analyzer.Finding (Finding, mapFindings)
import Language.Mulang.Analyzer.Analysis (Expectation, ExpectationResult(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)

analyseExpectations :: ExpectationsEvaluator -> Expression -> Maybe [Expectation] -> Finding [ExpectationResult]
analyseExpectations evaluator ast = mapFindings (analyseExpectation evaluator ast) . (fromMaybe [])

analyseExpectation :: ExpectationsEvaluator -> Expression -> Expectation -> Finding ExpectationResult
analyseExpectation evaluator ast e = fmap (ExpectationResult e) (compileAndEval e)
  where compileAndEval e = evaluator ast (compileExpectation e)

