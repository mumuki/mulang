{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Interpreter (
  newSample,
  expectationsSample,
  evaluate,
  Input(..),
  Output(..),
  SignatureGeneration,
  Expectation(..),
  ExpectationResult(..)) where

import Language.Mulang.Cli.Code
import Language.Mulang.Cli.Compiler

import GHC.Generics
import Data.Aeson

import Language.Mulang
import Language.Mulang.Inspector.Generic.Smell

data Input = Input {
  code :: Code,
  expectations :: [Expectation],
  signatureGeneration :: SignatureGeneration
} deriving (Show, Eq, Generic)

data Output = Output {
  results :: [ExpectationResult],
  smells :: [Expectation]
}  deriving (Show, Eq, Generic)

data SignatureGeneration = NoSignatureGeneration
                         | StructuredSignatureGeneration
                         | CodeSignatureGeneration deriving (Show, Eq, Generic)

data ExpectationResult = ExpectationResult {
  expectation :: Expectation,
  result :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Input
instance FromJSON Output
instance FromJSON ExpectationResult
instance FromJSON SignatureGeneration

instance ToJSON Output
instance ToJSON Input
instance ToJSON ExpectationResult
instance ToJSON SignatureGeneration

newSample :: Code -> Input
newSample code = Input code [] NoSignatureGeneration

expectationsSample :: Code -> [Expectation] -> Input
expectationsSample code es = (newSample code) { expectations = es }

evaluate :: Input -> Output
evaluate (Input code expectations _)
      | Just ast <- parseCode code = Output (evaluateExpectations expectations ast) (detectSmells ast)
      | otherwise = Output [] []

evaluateExpectations :: [Expectation] ->  Expression -> [ExpectationResult]
evaluateExpectations es content = map run es
  where
    run e = ExpectationResult e (compileAndEval e)
    compileAndEval e = (compile e) content

type NamedSmell = (String, Inspection)

detectSmells :: Expression -> [Expectation]
detectSmells code = concatMap (`runSingleSmellDetection` code) namedSmells

runSingleSmellDetection :: NamedSmell -> Expression -> [Expectation]
runSingleSmellDetection (name, inspection) code =
  map (smellyBindingToResult name) $ detect inspection code

namedSmells :: [NamedSmell]
namedSmells = [
  ("HasRedundantIf", hasRedundantIf),
  ("HasRedundantLambda", hasRedundantLambda),
  ("HasRedundantBooleanComparison", hasRedundantBooleanComparison),
  ("HasRedundantGuards", hasRedundantGuards),
  ("HasRedundantLocalVariableReturn", hasRedundantLocalVariableReturn),
  ("hasAssignmentReturn", hasAssignmentReturn),
  ("DoesNullTest", doesNullTest),
  ("DoesTypeTest", doesTypeTest),
  ("ReturnsNull", returnsNull)]

smellyBindingToResult smellName binding = Basic binding smellName


