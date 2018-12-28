module Language.Mulang.Analyzer (
  noSmells,
  allSmells,
  noSmellsBut,
  allSmellsBut,

  emptyDomainLanguage,
  emptyAnalysisSpec,

  emptyAnalysis,
  domainLanguageAnalysis,
  expectationsAnalysis,
  smellsAnalysis,
  signaturesAnalysis,

  emptyCompletedAnalysisResult,

  analyse,

  module Language.Mulang.Analyzer.Analysis) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis
import Language.Mulang.Analyzer.DomainLanguageCompiler (emptyDomainLanguage, compileDomainLanguage)
import Language.Mulang.Analyzer.ExpectationsAnalyzer (analyseExpectations)
import Language.Mulang.Analyzer.SampleParser (parseSample)
import Language.Mulang.Analyzer.SignaturesAnalyzer  (analyseSignatures)
import Language.Mulang.Analyzer.SmellsAnalyzer (analyseSmells)
import Language.Mulang.Analyzer.TestsAnalyzer  (analyseTests)
import Data.Maybe (fromMaybe)

--
-- Builder functions
--

noSmells :: Maybe SmellsSet
noSmells = Just $ NoSmells Nothing

noSmellsBut :: [Smell] -> Maybe SmellsSet
noSmellsBut = Just . NoSmells . Just

allSmells :: Maybe SmellsSet
allSmells = Just $ AllSmells Nothing

allSmellsBut :: [Smell] -> Maybe SmellsSet
allSmellsBut = Just . AllSmells . Just

emptyAnalysisSpec :: AnalysisSpec
emptyAnalysisSpec = AnalysisSpec Nothing Nothing Nothing Nothing Nothing Nothing

emptyAnalysis :: Sample -> Analysis
emptyAnalysis code = Analysis code emptyAnalysisSpec

domainLanguageAnalysis :: Sample -> DomainLanguage -> Analysis
domainLanguageAnalysis code domainLanguage = Analysis code (emptyAnalysisSpec { domainLanguage = Just domainLanguage, smellsSet = allSmells })

expectationsAnalysis :: Sample -> [Expectation] -> Analysis
expectationsAnalysis code es = Analysis code (emptyAnalysisSpec { expectations = Just es })

smellsAnalysis :: Sample -> Maybe SmellsSet -> Analysis
smellsAnalysis code set = Analysis code (emptyAnalysisSpec { smellsSet = set })

signaturesAnalysis :: Sample -> SignatureStyle -> Analysis
signaturesAnalysis code style = Analysis code (emptyAnalysisSpec { signatureAnalysisType = Just (StyledSignatures style) })

emptyCompletedAnalysisResult :: AnalysisResult
emptyCompletedAnalysisResult = AnalysisCompleted [] [] [] [] Nothing

--
-- Analysis running
--

analyse :: Analysis -> IO AnalysisResult
analyse (Analysis sample spec) = analyseSample (parseSample sample)
  where analyseSample (Right ast)    = analyseAst ast spec
        analyseSample (Left message) = return $ AnalysisFailed message

analyseAst :: Expression -> AnalysisSpec -> IO AnalysisResult
analyseAst ast spec = do
  language <- compileDomainLanguage (domainLanguage spec)
  testResults <- analyseTests ast (testAnalysisType spec)
  return $ AnalysisCompleted (analyseExpectations ast (expectations spec))
                             (analyseSmells ast language (smellsSet spec))
                             (analyseSignatures ast (signatureAnalysisType spec))
                             testResults
                             (analyzeIntermediateLanguage ast spec)

analyzeIntermediateLanguage :: Expression -> AnalysisSpec -> Maybe Expression
analyzeIntermediateLanguage ast spec
  | fromMaybe False (includeIntermediateLanguage spec) = Just ast
  | otherwise = Nothing
