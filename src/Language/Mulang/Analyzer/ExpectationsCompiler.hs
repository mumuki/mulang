{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import           Language.Mulang.Edl.Expectation

import           Language.Mulang (Inspection)
import           Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)
import qualified Language.Mulang.Analyzer.Analysis as A

import qualified Data.Map.Strict as Map
import           Data.List.Split (splitOn)

compileExpectation :: A.Expectation -> (Query, Inspection)
compileExpectation (A.Expectation s i) = (topQuery, compileTopQuery topQuery)
  where
    (inspectionParts, negator) = compileInspectionPartsAndNegator (splitOn ":" i)
    scope = compileScope (splitOn ":" s)
    baseQuery = compileCQuery inspectionParts
    topQuery = negator . scope $ baseQuery

compileInspectionPartsAndNegator :: [String] -> ([String], Query -> Query)
compileInspectionPartsAndNegator ("Not":ps) = (ps, Not)
compileInspectionPartsAndNegator ps         = (ps, id)

compileScope :: [String] -> CQuery -> Query
compileScope ["*"]                 q = Decontextualize q
compileScope ["Intransitive",name] q = Within name q
compileScope [name]                q = Through name q
compileScope _                     q = Decontextualize q

compileCQuery :: [String] -> CQuery
compileCQuery []                            = compileCQuery ["Parses","*"]
compileCQuery [verb]                        = compileCQuery [verb,"*"]
compileCQuery (verb:name:args)              | Map.member name nullaryMatchers = compileCQuery (verb:"*":name:args)
compileCQuery (verb:"WithReference":args)   = compileCQuery (verb:"*":"WithReference":args)
compileCQuery (verb:"WithChar":args)        = compileCQuery (verb:"*":"WithChar":args)
compileCQuery (verb:"WithNumber":args)      = compileCQuery (verb:"*":"WithNumber":args)
compileCQuery (verb:"WithString":args)      = compileCQuery (verb:"*":"WithString":args)
compileCQuery (verb:"WithSymbol":args)      = compileCQuery (verb:"*":"WithSymbol":args)
compileCQuery (verb:object:args)            = Inspection verb (compileBinding object) (compileMatcher args)

compileBinding :: String -> Predicate
compileBinding "*"          = Any
compileBinding ('^':name)   = Except name
compileBinding ('~':name)   = Like name
compileBinding ('=':name)   = Named name
compileBinding name         = Named name

compileMatcher :: [String] -> Matcher
compileMatcher = matching . f
  where
    matching [] = Unmatching
    matching xs = Matching xs

    readAsString value = read ("\""++value++"\"")

    f :: [String] -> [Clause]
    f (name:args)                  |  Just matcher <- Map.lookup name nullaryMatchers = matcher : f args
    f ("WithReference":value:args) =  IsReference (readAsString value) : f args
    f ("WithChar":value:args)      =  IsChar (read value) : f args
    f ("WithNumber":value:args)    =  IsNumber (read value) : f args
    f ("WithString":value:args)    =  IsString (read value) : f args
    f ("WithSymbol":value:args)    =  IsSymbol (readAsString value) : f args
    f []                           = []

nullaryMatchers =
  Map.fromList [
    ("WithAnything", IsAnything),
    ("WithFalse", IsFalse),
    ("WithLiteral", IsLiteral),
    ("WithLogic", IsLogic),
    ("WithMath", IsMath),
    ("WithNil", IsNil),
    ("WithNonliteral", IsNonliteral),
    ("WithTrue", IsTrue)
  ]
