{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Analyzer.Analysis (
  noSmells,
  allSmells,
  noSmellsBut,
  allSmellsBut,

  emptyDomainLanguage,
  emptyAnalysisSpec,

  emptyAnalysis,
  domainLanguageAnalysis,
  expectationsAnalysis,
  customExpectationsAnalysis,
  signaturesAnalysis,
  smellsAnalysis,
  testsAnalysis,

  customExpectationResult,
  emptyCompletedAnalysisResult,

  Expectation(..),

  Analysis(..),
  AnalysisSpec(..),
  AutocorrectionRules,
  CaseStyle(..),
  DomainLanguage(..),
  Fragment(..),
  Inspection,
  InterpreterOptions(..),
  Language(..),
  SignatureAnalysisType(..),
  SignatureStyle(..),
  Smell,
  SmellsSet(..),
  TestAnalysisType(..),
  TransformationSpec,
  TransformationOperation(..),
  TransformationScope(..),

  QueryResult,

  AnalysisResult,
  GenericAnalysisResult(..),
  ExpectationResult(..)) where

import GHC.Generics

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator (Operator)
import Language.Mulang.Edl.Expectation (Query)
import Language.Mulang.Transform.Normalizer (NormalizationOptions)
import Language.Mulang.Interpreter.Runner (TestResult)
import Data.Map.Strict (Map)

---
-- Common structures
--

type Smell = String
type Inspection = String
type AutocorrectionRules = Map Inspection Inspection

data Expectation = Expectation {
  binding :: String,
  inspection :: Inspection
} deriving (Show, Eq, Generic)

--
-- Analysis input structures
--

data Analysis = Analysis {
  sample :: Fragment,
  spec :: AnalysisSpec
} deriving (Show, Eq, Generic)

data AnalysisSpec = AnalysisSpec {
  expectations :: Maybe [Expectation],
  customExpectations :: Maybe String,
  smellsSet :: Maybe SmellsSet,
  signatureAnalysisType :: Maybe SignatureAnalysisType,
  testAnalysisType :: Maybe TestAnalysisType,
  transformationSpecs :: Maybe [TransformationSpec],
  domainLanguage :: Maybe DomainLanguage,
  includeOutputAst :: Maybe Bool,
  includeOutputIdentifiers :: Maybe Bool,
  originalLanguage :: Maybe Language,
  autocorrectionRules :: Maybe AutocorrectionRules,
  normalizationOptions :: Maybe NormalizationOptions
} deriving (Show, Eq, Generic)

data DomainLanguage = DomainLanguage {
  dictionaryFilePath :: Maybe FilePath,
  caseStyle :: Maybe CaseStyle,
  minimumIdentifierSize :: Maybe Int,
  jargon :: Maybe [String]
} deriving (Show, Eq, Generic)

data CaseStyle
  = CamelCase
  | SnakeCase
  | RubyCase deriving (Show, Eq, Generic)

data SmellsSet
  = NoSmells { include :: Maybe [Smell] }
  | AllSmells { exclude :: Maybe [Smell] } deriving (Show, Eq, Generic)

data SignatureAnalysisType
  = NoSignatures
  | DefaultSignatures
  | StyledSignatures { style :: SignatureStyle } deriving (Show, Eq, Generic)

data SignatureStyle
  = MulangStyle
  | UntypedCStyle
  | HaskellStyle
  | PrologStyle deriving (Show, Eq, Generic)

data Fragment
  = MulangSample { ast :: Expression }
  | CodeSample { language :: Language, content :: Code } deriving (Show, Eq, Generic)

data InterpreterOptions = InterpreterOptions {
  strictReturns :: Maybe Bool,
  strictShortCircuit :: Maybe Bool
} deriving (Show, Eq, Generic)

data TestAnalysisType
  = NoTests
  | EmbeddedTests { interpreterOptions :: Maybe InterpreterOptions }
  | ExternalTests {
      test :: Fragment,
      extra :: Maybe Fragment,
      interpreterOptions :: Maybe InterpreterOptions
    } deriving (Show, Eq, Generic)

type TransformationSpec = [TransformationOperation]

data TransformationOperation
  = Alias (Map String Operator)
  | Crop Inspection
  | CropAt TransformationScope Inspection
  | Normalize NormalizationOptions
  | RenameVariables
  | Replace Inspection Expression
  | ReplaceAt TransformationScope Inspection Expression deriving (Show, Eq, Generic)

data TransformationScope = GlobalScope | LocalScope deriving (Show, Eq, Generic)

data Language
  =  Json
  |  Java
  |  JavaScript
  |  Prolog
  |  Haskell
  |  Python
  |  Python2
  |  Python3
  |  Ruby
  |  Php
  |  C
  deriving (Show, Eq, Generic)

--
-- Analysis Intermidiate structures
--

type QueryResult = (Query, ExpectationResult)

type AnalysisResult = GenericAnalysisResult Expression
--
-- Analysis Output structures
--
data GenericAnalysisResult a
  = AnalysisCompleted {
      expectationResults :: [ExpectationResult],
      smells :: [Expectation],
      signatures :: [Code],
      testResults :: [TestResult],
      outputAst :: Maybe a,
      outputIdentifiers :: Maybe ([String], [String]),
      transformedAsts :: Maybe [a] }
  | AnalysisFailed { reason :: String } deriving (Show, Eq, Generic)

data ExpectationResult = ExpectationResult {
  expectation :: Expectation,
  result :: Bool
} deriving (Show, Eq, Generic)


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
emptyAnalysisSpec = AnalysisSpec {
    expectations = Nothing,
    customExpectations = Nothing,
    smellsSet = Nothing,
    signatureAnalysisType = Nothing,
    testAnalysisType = Nothing,
    transformationSpecs = Nothing,
    domainLanguage = Nothing,
    includeOutputAst = Nothing,
    includeOutputIdentifiers = Nothing,
    originalLanguage = Nothing,
    autocorrectionRules = Nothing,
    normalizationOptions = Nothing
  }

emptyAnalysis :: Fragment -> Analysis
emptyAnalysis code = Analysis code emptyAnalysisSpec

domainLanguageAnalysis :: Fragment -> DomainLanguage -> Analysis
domainLanguageAnalysis code domainLanguage = Analysis code (emptyAnalysisSpec { domainLanguage = Just domainLanguage, smellsSet = allSmells })

customExpectationsAnalysis :: Fragment -> String -> Analysis
customExpectationsAnalysis code es = Analysis code (emptyAnalysisSpec { customExpectations = Just es })

expectationsAnalysis :: Fragment -> [Expectation] -> Analysis
expectationsAnalysis code es = Analysis code (emptyAnalysisSpec { expectations = Just es })

smellsAnalysis :: Fragment -> Maybe SmellsSet -> Analysis
smellsAnalysis code set = Analysis code (emptyAnalysisSpec { smellsSet = set })

signaturesAnalysis :: Fragment -> SignatureStyle -> Analysis
signaturesAnalysis code style = Analysis code (emptyAnalysisSpec { signatureAnalysisType = Just (StyledSignatures style) })

testsAnalysis :: Fragment -> TestAnalysisType -> Analysis
testsAnalysis code testAnalysisType = Analysis code (emptyAnalysisSpec { testAnalysisType = Just testAnalysisType })

emptyCompletedAnalysisResult :: AnalysisResult
emptyCompletedAnalysisResult = AnalysisCompleted [] [] [] [] Nothing Nothing Nothing

emptyDomainLanguage :: DomainLanguage
emptyDomainLanguage = DomainLanguage Nothing Nothing Nothing Nothing

customExpectationResult :: String -> Bool -> ExpectationResult
customExpectationResult title result = ExpectationResult (Expectation "<<custom>>" title) result
