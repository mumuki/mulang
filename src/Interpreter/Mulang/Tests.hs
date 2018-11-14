module Interpreter.Mulang.Tests
  ( getTests
  , TestsMonad
  , MuTest(..)
  ) where

import           Control.Monad (forM)
import           Control.Monad.Writer.Strict
import           Data.List (intercalate)

import qualified Language.Mulang as Mu

data MuTest = MuTest { description :: [String]
                     , body :: Mu.Expression
                     } deriving (Show)

getTests :: Mu.Expression -> [MuTest]
getTests expr = execWriter $ getAllTestsFromExpr [] expr

type TestsMonad a = Writer [MuTest] a

getAllTestsFromExpr :: [String] -> Mu.Expression -> Writer [MuTest] ()
getAllTestsFromExpr s (Mu.Application (Mu.Reference "describe") [Mu.MuString desc, Mu.Lambda [] f]) = do
  getAllTestsFromExpr (s ++ [desc]) f
getAllTestsFromExpr s (Mu.Sequence expressions) =
  void $ forM expressions (getAllTestsFromExpr s)
getAllTestsFromExpr s (Mu.Application (Mu.Reference "it") [Mu.MuString desc, Mu.Lambda [] f]) = do
  tell $ [ MuTest { description = s ++ [desc]
                  , body = f
                  }
         ]
getAllTestsFromExpr s e =
  error $ "Unknown expression: " ++ show e ++ "\nIn " ++ intercalate " > " s



