module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseCustomExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.Finding
import Language.Mulang.Analyzer.ExpectationsEvaluator (ExpectationsEvaluator)
import Language.Mulang.Analyzer.Analysis (customExpectationResult, ExpectationResult(..))
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseCustomExpectations :: ExpectationsEvaluator -> Expression -> Maybe String -> Finding [ExpectationResult]
analyseCustomExpectations evaluator ast  = fromMaybe (return []) . fmap (analyseCustomExpectations' evaluator ast)

analyseCustomExpectations' :: ExpectationsEvaluator -> Expression -> String -> Finding [ExpectationResult]
analyseCustomExpectations' evaluator ast = mapFindings (analyseExpectation evaluator ast) . parseExpectations

analyseExpectation :: ExpectationsEvaluator -> Expression -> Expectation -> Finding ExpectationResult
analyseExpectation evaluator ast (Expectation name q) = fmap (customExpectationResult name) compileAndEval
  where compileAndEval = evaluator ast (compileTopQuery q)
