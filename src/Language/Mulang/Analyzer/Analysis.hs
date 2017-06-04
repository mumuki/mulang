{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Analyzer.Analysis (
  Expectation(..),
  BindingPattern(..),

  Analysis(..),
  AnalysisSpec(..),
  SignatureAnalysisType(..),
  SignatureStyle(..),
  CodeSample(..),
  Language(..),

  AnalysisResult(..),
  ExpectationResult(..)) where

import GHC.Generics

import Data.Aeson

import Language.Mulang (Code)

--
-- Common structures
--

data Expectation =  Advanced {
                      subject :: [String] ,
                      verb :: String,
                      object :: BindingPattern,
                      transitive :: Bool,
                      negated :: Bool
                    }
                  | Basic {
                    binding :: String,
                    inspection :: String
                  } deriving (Show, Eq, Generic)

data BindingPattern = Named { exactName :: String }
                    | Like { similarName :: String }
                    | Anyone deriving (Show, Eq, Generic)

instance FromJSON Expectation
instance FromJSON BindingPattern

instance ToJSON Expectation
instance ToJSON BindingPattern

--
-- Analysis input structures
--

data Analysis = Analysis {
  sample :: CodeSample,
  spec :: AnalysisSpec
} deriving (Show, Eq, Generic)

data AnalysisSpec = AnalysisSpec {
  expectations :: [Expectation],
  signatureAnalysisType :: SignatureAnalysisType
} deriving (Show, Eq, Generic)

data SignatureAnalysisType
  = NoSignatures
  | DefaultSignatures
  | StyledSignatures { style :: SignatureStyle } deriving (Show, Eq, Generic)

data SignatureStyle
  = MulangStyle
  | UntypedCStyle
  | HaskellStyle
  | PrologStyle deriving (Show, Eq, Generic)

data CodeSample = CodeSample {
  language :: Language,
  content :: Code
} deriving (Show, Eq, Generic)

data Language =  Mulang
              |  Json
              |  JavaScript
              |  Prolog
              |  GobstonesAst
              |  Gobstones
              |  Haskell deriving (Show, Eq, Generic)

instance FromJSON Analysis
instance FromJSON AnalysisSpec
instance FromJSON SignatureAnalysisType
instance FromJSON SignatureStyle
instance FromJSON CodeSample
instance FromJSON Language

--
-- Analysis Output structures
--

data AnalysisResult = AnalysisCompleted {
                        expectationResults :: [ExpectationResult],
                        smells :: [Expectation],
                        signatures :: [Code]
                      }
                    | AnalysisFailed { reason :: String } deriving (Show, Eq, Generic)

data ExpectationResult = ExpectationResult {
  expectation :: Expectation,
  result :: Bool
} deriving (Show, Eq, Generic)

instance ToJSON AnalysisResult
instance ToJSON ExpectationResult
