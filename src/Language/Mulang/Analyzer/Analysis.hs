{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Analyzer.Analysis (
  noDomainLanguageViolations,

  Expectation(..),
  BindingPattern(..),

  Analysis(..),
  AnalysisSpec(..),
  SmellsSet(..),
  DomainLanguage(..),
  CaseStyle(..),
  Smell(..),
  SignatureAnalysisType(..),
  SignatureStyle(..),
  Sample(..),
  Language(..),

  AnalysisResult(..),
  ExpectationResult(..),
  DomainLanguageViolations(..)) where

import GHC.Generics

import Language.Mulang.Ast
import Language.Mulang.Binding (Binding)

---
-- Common structures
--

type BasicInspection = String
type Inspection = String

data Expectation =  Advanced {
                      subject :: [Binding] ,
                      verb :: Inspection,
                      object :: BindingPattern,
                      transitive :: Bool,
                      negated :: Bool
                    }
                  | Basic {
                    binding :: String,
                    inspection :: BasicInspection
                  } deriving (Show, Eq, Generic)

data BindingPattern
  = Named { exactName :: Binding }
  | Like { similarName :: Binding }
  | Anyone deriving (Show, Eq, Generic)

--
-- Analysis input structures
--

data Analysis = Analysis {
  sample :: Sample,
  spec :: AnalysisSpec
} deriving (Show, Eq, Generic)

data AnalysisSpec = AnalysisSpec {
  expectations :: [Expectation],
  smellsSet :: SmellsSet,
  signatureAnalysisType :: SignatureAnalysisType,
  domainLanguage :: Maybe DomainLanguage
} deriving (Show, Eq, Generic)

data DomainLanguage
  = DomainLanguage {
      dictionaryFilePath :: Maybe FilePath,
      caseStyle :: Maybe CaseStyle,
      strict :: Maybe Bool,
      minimumBindingSize :: Maybe Int
    }  deriving (Show, Eq, Generic)

data CaseStyle
  = CamelCase
  | SnakeCase deriving (Show, Eq, Generic)

data SmellsSet
  = NoSmells
  | OnlySmells { include :: [Smell] }
  | AllSmells { exclude :: [Smell] } deriving (Show, Eq, Generic)

data Smell
  = HasRedundantIf
  | HasRedundantLambda
  | HasRedundantBooleanComparison
  | HasRedundantGuards
  | HasRedundantLocalVariableReturn
  | HasAssignmentReturn
  | DoesNullTest
  | DoesTypeTest
  | IsLongCode
  | ReturnsNull
  | HasRedundantParameter
  | HasCodeDuplication deriving (Show, Eq, Enum, Bounded, Generic)

data SignatureAnalysisType
  = NoSignatures
  | DefaultSignatures
  | StyledSignatures { style :: SignatureStyle } deriving (Show, Eq, Generic)

data SignatureStyle
  = MulangStyle
  | UntypedCStyle
  | HaskellStyle
  | PrologStyle deriving (Show, Eq, Generic)

data Sample
  = MulangSample { ast :: Expression }
  | CodeSample { language :: Language, content :: Code } deriving (Show, Eq, Generic)

data Language
  =  Json
  |  JavaScript
  |  Prolog
  |  GobstonesAst
  |  Gobstones
  |  Haskell deriving (Show, Eq, Generic)

--
-- Analysis Output structures
--

data AnalysisResult
  = AnalysisCompleted { expectationResults :: [ExpectationResult],
                        smells :: [Expectation],
                        signatures :: [Code],
                        domainLanguageViolations :: DomainLanguageViolations }
  | AnalysisFailed { reason :: String } deriving (Show, Eq, Generic)

data ExpectationResult = ExpectationResult {
  expectation :: Expectation,
  result :: Bool
} deriving (Show, Eq, Generic)

data DomainLanguageViolations
  = DomainLanguageViolations {
      tooShortBindings :: [Binding],
      misspelledBindings :: [Binding]
    } deriving (Show, Eq, Generic)


noDomainLanguageViolations :: DomainLanguageViolations
noDomainLanguageViolations = DomainLanguageViolations [] []
