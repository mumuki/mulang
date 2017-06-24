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

import qualified Language.Mulang.DomainLanguage as DL
import Text.Inflections.Tokenizer (camelCase, snakeCase)
import Text.Dictionary (fromFile, toDictionary)



--
-- Builder functions
--
noSmells :: SmellsSet
noSmells = NoSmells

onlySmells :: SmellsSet
onlySmells = OnlySmells []

allSmells :: SmellsSet
allSmells = AllSmells []

emptyDomainLanguage :: DomainLanguage
emptyDomainLanguage = DomainLanguage Nothing Nothing Nothing

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

compileDomainLanguage :: Maybe DomainLanguage -> IO DL.DomainLanguage
compileDomainLanguage Nothing                                 = compileDomainLanguage (Just emptyDomainLanguage)
compileDomainLanguage (Just (DomainLanguage path style size)) = do
  dictionary <- compileDictionay path
  return $ DL.DomainLanguage dictionary (compileStyle style) (compileSize size)

  where
    compileDictionay (Just path) = fromFile path
    compileDictionay _           = return $ toDictionary []

    compileSize (Just n) = n
    compileSize _        = 3

    compileStyle (Just SnakeCase) = snakeCase
    compileStyle _                = camelCase


