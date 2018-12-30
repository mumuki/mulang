module Language.Mulang.Interpreter.Tests (
  getTests,
  MuTest(..)) where

import           Data.List (intercalate)

import  Language.Mulang.Ast (Expression(..))

data MuTest = MuTest { description :: [String], body :: Expression } deriving (Show)

getTests :: Expression -> [MuTest]
getTests expr = getAllTestsFromExpr [] expr

getAllTestsFromExpr :: [String] -> Expression -> [MuTest]
getAllTestsFromExpr s (Test (MuString desc) f)      = [MuTest (s ++ [desc]) f]
getAllTestsFromExpr s (TestGroup (MuString desc) f) = getAllTestsFromExpr (s ++ [desc]) f
getAllTestsFromExpr s (Sequence expressions)        = concatMap (getAllTestsFromExpr s) expressions
getAllTestsFromExpr s e                             = error $ "Unknown expression: " ++ show e ++ "\nIn " ++ intercalate " > " s



