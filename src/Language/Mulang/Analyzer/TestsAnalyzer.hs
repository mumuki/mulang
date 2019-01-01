module Language.Mulang.Analyzer.TestsAnalyzer (
  analyseTests) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (TestAnalysisType(..))
import Language.Mulang.Analyzer.FragmentParser (parseFragment')
import Language.Mulang.Builder (merge)
import Language.Mulang.Interpreter.Runner (runTests, TestResult(..))

import Data.Maybe (fromMaybe)

analyseTests :: Expression -> Maybe TestAnalysisType -> IO [TestResult]
analyseTests e analysis = analyseTests' e (fromMaybe NoTests analysis)

analyseTests' _ NoTests                      = return []
analyseTests' e (EmbeddedTests _)            = runTests e e
analyseTests' e (ExternalTests test extra _) = runTests (merge e extraFragment) testFragment
    where
      testFragment  = parseFragment' test
      extraFragment = fromMaybe None . fmap parseFragment' $ extra
