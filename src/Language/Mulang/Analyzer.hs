module Language.Mulang.Analyzer (
  noSmells,
  onlySmells,
  allSmells,

  emptyDomainLanguage,
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
import Language.Mulang.Analyzer.DomainLanguageCompiler (emptyDomainLanguage, compileDomainLanguage)

--
-- Builder functions
--
noSmells :: SmellsSet
noSmells = NoSmells

onlySmells :: SmellsSet
onlySmells = OnlySmells []

allSmells :: SmellsSet
allSmells = AllSmells []

emptyAnalysisSpec :: AnalysisSpec
emptyAnalysisSpec = AnalysisSpec [] noSmells NoSignatures Nothing

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
analyse :: Analysis -> IO AnalysisResult
analyse (Analysis sample spec)
      | Just ast <- parseSample sample = analyseAst ast spec
      | otherwise = return $ AnalysisFailed "Sample code parsing error"

analyseAst :: Expression -> AnalysisSpec -> IO AnalysisResult
analyseAst ast spec = do
  language <- compileDomainLanguage (domainLanguage spec)
  return $ AnalysisCompleted (analyseExpectations ast (expectations spec))
                             (analyseSmells ast language (smellsSet spec))
                             (analyseSignatures ast (signatureAnalysisType spec))

