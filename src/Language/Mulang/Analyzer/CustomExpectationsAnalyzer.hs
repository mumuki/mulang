module Language.Mulang.Analyzer.CustomExpectationsAnalyzer (
  analyseCustomExpectations) where

import Data.Maybe (fromMaybe)

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (customExpectationResult, ExpectationResult(..))
import Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)

import Language.Mulang.Edl (parseExpectations, Expectation (..))

analyseCustomExpectations :: Expression -> Maybe String -> [ExpectationResult]
analyseCustomExpectations ast  = fromMaybe [] . fmap (map (runExpectation ast) . parseExpectations)

runExpectation :: Expression -> Expectation -> ExpectationResult
runExpectation ast (Expectation name q) = customExpectationResult name (compileTopQuery q ast)

