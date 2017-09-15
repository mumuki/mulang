module Language.Mulang.Analyzer.Analysis.Json () where

import Data.Aeson
import Language.Mulang
import Language.Mulang.Analyzer.Analysis

instance FromJSON Analysis
instance FromJSON AnalysisSpec

instance FromJSON Expectation

instance FromJSON SmellsSet
instance FromJSON DomainLanguage
instance FromJSON CaseStyle

instance FromJSON SignatureAnalysisType
instance FromJSON SignatureStyle

instance FromJSON Sample
instance FromJSON Language

instance FromJSON Equation
instance FromJSON EquationBody
instance FromJSON Expression
instance FromJSON Pattern
instance FromJSON ComprehensionStatement

instance ToJSON AnalysisResult
instance ToJSON ExpectationResult

instance ToJSON Expectation

instance ToJSON Equation
instance ToJSON EquationBody
instance ToJSON Expression
instance ToJSON Pattern
instance ToJSON ComprehensionStatement

