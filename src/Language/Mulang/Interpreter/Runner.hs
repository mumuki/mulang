{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Interpreter.Runner (
  runTests,
  runTestsForDir,
  TestResult(..),
  TestStatus(..)) where

import GHC.Generics

import           Control.Monad (forM)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict
import           Control.Monad.Cont (callCC)
import           Data.Maybe (fromMaybe, fromJust)
import           Data.List (intercalate)

import qualified Language.Mulang.Ast as Mu
import Language.Mulang.Interpreter
import Language.Mulang.Parsers (MaybeParser)

data TestResult
  = TestResult { description :: [String], status :: TestStatus } deriving (Generic, Show, Eq)

data TestStatus
  = Success
  | Failure String
  deriving (Generic, Show, Eq)

data MuTest = MuTest [String] Mu.Expression deriving (Show)

runTestsForDir :: String -> String -> MaybeParser -> IO ()
runTestsForDir solutionPath testPath parse = do
  solution <- fromJust . parse <$> readFile solutionPath
  tests <- fromJust . parse <$> readFile testPath
  results <- runTests solution tests
  forM_ results $ \result -> case result of
    (TestResult desc Success) -> do
      putStrLn $ "PASS : " ++ intercalate " > " desc
    (TestResult desc (Failure reason)) -> do
      putStrLn $ "FAIL : " ++ intercalate " > " desc
      putStrLn $ "       " ++ show reason

runTests :: Mu.Expression -> Mu.Expression -> IO [TestResult]
runTests code = runTests' code . getTests

runTests' :: Mu.Expression -> [MuTest] -> IO [TestResult]
runTests' expr tests = do
  (_ref, context) <- eval defaultContext expr
  forM tests $ \(MuTest desc testExpr) -> do
    (exceptionRef, testContext) <- eval' context $ do
      _ <- callCC $ \raiseCallback -> do
        put (context { currentRaiseCallback = raiseCallback })
        evalExpr testExpr
        return nullRef
      fromMaybe nullRef <$> gets currentException
    let exception = dereference' (globalObjects testContext) exceptionRef
    case exception of
      MuNull -> return $ TestResult desc Success
      v -> return $ TestResult desc (Failure $ show v)

getTests :: Mu.Expression -> [MuTest]
getTests expr = getAllTestsFromExpr [] expr

getAllTestsFromExpr :: [String] -> Mu.Expression -> [MuTest]
getAllTestsFromExpr s (Mu.Test (Mu.MuString desc) f)      = [MuTest (s ++ [desc]) f]
getAllTestsFromExpr s (Mu.TestGroup (Mu.MuString desc) f) = getAllTestsFromExpr (s ++ [desc]) f
getAllTestsFromExpr s (Mu.Sequence expressions)        = concatMap (getAllTestsFromExpr s) expressions
getAllTestsFromExpr s e                                = error $ "Unknown expression: " ++ show e ++ "\nIn " ++ intercalate " > " s
