{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import           Language.Mulang.Edl.Expectation

import           Language.Mulang (Inspection)
import           Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)
import qualified Language.Mulang.Analyzer.Analysis as A

import qualified Data.Map.Strict as Map
import           Data.List.Split (splitOn)
import           Control.Monad.Except
import           Data.Either (either)

type Compilation e = Either String e

compileExpectation :: A.Expectation -> Inspection
compileExpectation (A.Expectation s i) = either (const (const True)) id (fmap (compileTopQuery . negator . scope) baseQuery)
  where
    (inspectionParts, negator) = compileInspectionPartsAndNegator (splitOn ":" i)
    scope = compileScope (splitOn ":" s)
    baseQuery = compileCQuery inspectionParts

compileInspectionPartsAndNegator :: [String] -> ([String], Query -> Query)
compileInspectionPartsAndNegator ("Not":ps) = (ps, Not)
compileInspectionPartsAndNegator ps         = (ps, id)

compileScope :: [String] -> CQuery -> Query
compileScope ["*"]                 q = Decontextualize q
compileScope ["Intransitive",name] q = Within name q
compileScope [name]                q = Through name q
compileScope _                     q = Decontextualize q

compileCQuery :: [String] -> Compilation CQuery
compileCQuery []                            = compileCQuery ["Parses","*"]
compileCQuery [verb]                        = compileCQuery [verb,"*"]
compileCQuery [verb,name]                   | Map.member name nullaryMatchers = compileCQuery [verb,"*",name]
compileCQuery (verb:"WithChar":args)        = compileCQuery (verb:"*":"WithChar":args)
compileCQuery (verb:"WithNumber":args)      = compileCQuery (verb:"*":"WithNumber":args)
compileCQuery (verb:"WithString":args)      = compileCQuery (verb:"*":"WithString":args)
compileCQuery (verb:"WithSymbol":args)      = compileCQuery (verb:"*":"WithSymbol":args)
compileCQuery (verb:object:args)            = fmap (Inspection verb (compileBinding object)) (compileMatcher args)

compileBinding :: String -> Predicate
compileBinding "*"          = Any
compileBinding ('^':name)   = Except name
compileBinding ('~':name)   = Like name
compileBinding ('=':name)   = Named name
compileBinding name         = Named name

compileMatcher :: [String] -> Compilation Matcher
compileMatcher = fmap matching . f
  where
    matching [] = Unmatching
    matching xs = Matching xs

    f :: [String] -> Compilation [Clause]
    f (name:args)        | Just matcher <- Map.lookup name nullaryMatchers = fmap (matcher :) (f args)
    f (name:value:args)  | Just matcher <- Map.lookup name binaryMatchers =  fmap (matcher value :) (f args)
    f []                 = return []
    f (other:args)       = throwError $ "unsupported matcher " ++ other ++ " with args " ++ show args

binaryMatchers =
  Map.fromList [
    ("WithChar", (IsChar . read)),
    ("WithNumber", (IsNumber . read)),
    ("WithString", (IsString . read)),
    ("WithSymbol", (\ v-> IsSymbol (read ("\""++v++"\""))))
  ]

nullaryMatchers =
  Map.fromList [
    ("WithFalse", IsFalse),
    ("WithLiteral", IsLiteral),
    ("WithLogic", IsLogic),
    ("WithMath", IsMath),
    ("WithNil", IsNil),
    ("WithNonliteral", IsNonliteral),
    ("WithTrue", IsTrue)
  ]
