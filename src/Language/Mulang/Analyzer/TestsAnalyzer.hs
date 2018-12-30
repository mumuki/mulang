module Language.Mulang.Analyzer.TestsAnalyzer (
  analyseTests) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (TestAnalysisType(..))
import Language.Mulang.Interpreter.Runner (runTests, TestResult(..))
import Language.Mulang.Interpreter.Tests (getTests) -- TODO: Merge getTests with Runner, Test data with AST and remove this module


import Data.Maybe (fromMaybe)

analyseTests :: Expression -> Maybe TestAnalysisType -> IO [TestResult]
analyseTests e analysis = analyseTests' e (fromMaybe IgnoreTests analysis)

analyseTests' _ IgnoreTests    = return []
-- TODO: partition expression into test and main. Now original tests are passed alongside the main code
-- TODO: create TestResult natively in runTests
analyseTests' e (RunTests _ _) = runTests e . getTests $ e
