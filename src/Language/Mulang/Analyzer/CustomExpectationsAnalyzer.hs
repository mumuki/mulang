module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseCustomExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.Finding
import Language.Mulang.Analyzer.Analysis (customExpectationResult, ExpectationResult(..))
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseCustomExpectations :: Expression -> Maybe String -> Finding [ExpectationResult]
analyseCustomExpectations ast  = fromMaybe (return []) . fmap (analyseCustomExpectations' ast)

analyseCustomExpectations' :: Expression -> String -> Finding [ExpectationResult]
analyseCustomExpectations' ast = mapFindings (analyseExpectation ast) . parseExpectations

analyseExpectation :: Expression -> Expectation -> Finding ExpectationResult
analyseExpectation ast (Expectation name q) = fmap (customExpectationResult name) compileAndEval
  where compileAndEval = fmap ($ ast) (compileTopQuery q)
