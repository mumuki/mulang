module Language.Mulang.Analyzer.TestsAnalyzer (
  analyseTests) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (TestAnalysisType(..))
import Language.Mulang.Analyzer.FragmentParser (parseFragment')
import Language.Mulang.Builder (merge)
import Language.Mulang.Interpreter.Runner (runTests, TestResult(..))
import Language.Mulang.Transform.Normalizer (NormalizationOptions)

import Data.Maybe (fromMaybe)

analyseTests :: Expression -> Maybe TestAnalysisType -> Maybe NormalizationOptions -> IO [TestResult]
analyseTests e analysis options = analyseTests' e (fromMaybe NoTests analysis) options

analyseTests' _ NoTests                      _       = return []
analyseTests' e (EmbeddedTests _)            _       = runTests e e
analyseTests' e (ExternalTests test extra _) options = runTests (merge e extraFragment) testFragment
    where
      testFragment  = parse test
      extraFragment = fromMaybe None . fmap parse $ extra
      parse = parseFragment' options
