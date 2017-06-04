module Language.Mulang.Analyzer (
  noSmells,
  allSmells,

  emptyAnalysisSpec,

  emptyAnalysis,
  expectationsAnalysis,
  smellsAnalysis,
  signaturesAnalysis,

  analyse,

  module Language.Mulang.Analyzer.Analysis) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis
import Language.Mulang.Analyzer.SampleParser (parseSample)
import Language.Mulang.Analyzer.SignaturesAnalyzer  (analyseSignatures)
import Language.Mulang.Analyzer.ExpectationsAnalyzer (analyseExpectations)
import Language.Mulang.Analyzer.SmellsAnalyzer (analyseSmells)

--
-- Builder functions
--
noSmells :: SmellsSet
noSmells = NoSmells []

allSmells :: SmellsSet
allSmells = AllSmells []

emptyAnalysisSpec :: AnalysisSpec
emptyAnalysisSpec = AnalysisSpec [] noSmells NoSignatures

emptyAnalysis :: Sample -> Analysis
emptyAnalysis code = Analysis code emptyAnalysisSpec

expectationsAnalysis :: Sample -> [Expectation] -> Analysis
expectationsAnalysis code es = Analysis code (emptyAnalysisSpec { expectations = es })

smellsAnalysis :: Sample -> SmellsSet -> Analysis
smellsAnalysis code set = Analysis code (emptyAnalysisSpec { smellsSet = set })

signaturesAnalysis :: Sample -> SignatureStyle -> Analysis
signaturesAnalysis code style = Analysis code (emptyAnalysisSpec { signatureAnalysisType = StyledSignatures style })


--
-- Analysis running
--

analyse :: Analysis -> AnalysisResult
analyse (Analysis sample spec)
      | Just ast <- parseSample sample = analyseAst ast spec
      | otherwise = AnalysisFailed "Sample code parsing error"

analyseAst :: Expression -> AnalysisSpec -> AnalysisResult
analyseAst ast spec =
  AnalysisCompleted (analyseExpectations ast (expectations spec))
                    (analyseSmells ast)
                    (analyseSignatures ast (signatureAnalysisType spec))