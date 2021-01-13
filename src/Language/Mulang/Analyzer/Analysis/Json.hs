{-# LANGUAGE OverloadedStrings #-}

module Language.Mulang.Analyzer.Analysis.Json () where

import           Data.Aeson
import           Language.Mulang
import           Language.Mulang.Analyzer.Analysis
import           Language.Mulang.Transform.Normalizer (NormalizationOptions (..), SequenceSortMode, unnormalized)
import           Language.Mulang.Interpreter.Runner (TestResult, TestStatus)

instance FromJSON Analysis
instance FromJSON AnalysisSpec

instance FromJSON Expectation

instance FromJSON SmellsSet
instance FromJSON DomainLanguage
instance FromJSON CaseStyle

instance FromJSON SignatureAnalysisType
instance FromJSON SignatureStyle

instance FromJSON NormalizationOptions where
    parseJSON = withObject "NormalizationOptions" $ \v -> NormalizationOptions
        <$> v .:?  "convertObjectVariableIntoObject"              .!= convertObjectVariableIntoObject d
        <*> v .:? "convertLambdaVariableIntoFunction"             .!= convertLambdaVariableIntoFunction d
        <*> v .:? "convertObjectLevelFunctionIntoMethod"          .!= convertObjectLevelFunctionIntoMethod d
        <*> v .:? "convertObjectLevelLambdaVariableIntoMethod"    .!= convertObjectLevelLambdaVariableIntoMethod d
        <*> v .:? "convertObjectLevelVariableIntoAttribute"       .!= convertObjectLevelVariableIntoAttribute d
        <*> v .:? "convertObjectIntoDict"                         .!= convertObjectIntoDict d
        <*> v .:? "sortSequenceDeclarations"                      .!= sortSequenceDeclarations d
        <*> v .:? "insertImplicitReturn"                          .!= insertImplicitReturn d
        <*> v .:? "compactSequences"                              .!= compactSequences d
        <*> v .:? "trimSequences"                                 .!= trimSequences d
        <*> v .:? "sortCommutativeApplications"                   .!= sortCommutativeApplications d
          where d = unnormalized
instance FromJSON SequenceSortMode

instance FromJSON Fragment
instance FromJSON Language

instance FromJSON Equation
instance FromJSON EquationBody
instance FromJSON Expression
instance FromJSON Pattern
instance FromJSON Statement
instance FromJSON Type
instance FromJSON Assertion
instance FromJSON TestAnalysisType
instance FromJSON TransformationOperation
instance FromJSON InterpreterOptions
instance FromJSON Operator

instance ToJSON a => ToJSON (GenericAnalysisResult a)
instance ToJSON ExpectationResult

instance ToJSON Expectation

instance ToJSON Equation
instance ToJSON EquationBody
instance ToJSON Expression
instance ToJSON Pattern
instance ToJSON Statement
instance ToJSON Type
instance ToJSON Assertion
instance ToJSON TestResult
instance ToJSON TestStatus
instance ToJSON Operator
