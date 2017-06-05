module Language.Mulang.Analyzer.Analysis.Json () where

import Data.Aeson
import Data.Aeson.TH

import Language.Mulang
import Language.Mulang.Analyzer.Analysis

untaggedJsonOptions :: Options
untaggedJsonOptions = defaultOptions { sumEncoding = UntaggedValue }

instance FromJSON Expectation where
    parseJSON = genericParseJSON untaggedJsonOptions

instance FromJSON BindingPattern where
    parseJSON = genericParseJSON untaggedJsonOptions

instance ToJSON Expectation where
    toEncoding = genericToEncoding untaggedJsonOptions

instance ToJSON BindingPattern where
    toEncoding = genericToEncoding untaggedJsonOptions

instance FromJSON Analysis
instance FromJSON AnalysisSpec

instance FromJSON SmellsSet where
    parseJSON = genericParseJSON untaggedJsonOptions

instance FromJSON Smell

instance FromJSON SignatureAnalysisType where
    parseJSON = genericParseJSON untaggedJsonOptions

instance FromJSON SignatureStyle

instance FromJSON Sample where
    parseJSON = genericParseJSON untaggedJsonOptions

instance FromJSON Language

instance FromJSON Equation
instance FromJSON EquationBody
instance FromJSON Expression
instance FromJSON Pattern
instance FromJSON ComprehensionStatement

instance ToJSON AnalysisResult
instance ToJSON ExpectationResult
