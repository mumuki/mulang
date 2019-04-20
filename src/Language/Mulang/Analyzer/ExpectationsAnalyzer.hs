module Language.Mulang.Analyzer.ExpectationsAnalyzer (
  analyseExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (Language, Expectation, ExpectationResult(..))
import Language.Mulang.Analyzer.ExpectationsCompiler (compileExpectation)

analyseExpectations :: Maybe Language -> Expression -> Maybe [Expectation] -> [ExpectationResult]
analyseExpectations language content = map (analyseExpectation language content) . (fromMaybe [])

analyseExpectation :: Maybe Language -> Expression -> Expectation -> ExpectationResult
analyseExpectation language ast e = ExpectationResult e (compileAndEval e)
  where compileAndEval e = (compileExpectation language e) ast

