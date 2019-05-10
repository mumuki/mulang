{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import           Language.Explang.Expectation

import           Language.Mulang (Inspection)
import qualified Language.Mulang.Analyzer.ExplangExpectationsCompiler as EC
import qualified Language.Mulang.Analyzer.Analysis as A

import Data.List.Split (splitOn)

compileExpectation :: A.Expectation -> Inspection
compileExpectation (A.Expectation s i) = EC.compileExpectation . negator . scope $ baseQuery -- TODO move to explang
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

compileCQuery :: [String] -> CQuery
compileCQuery []                            = compileCQuery ["Parses","*"]
compileCQuery [verb]                        = compileCQuery [verb,"*"]
compileCQuery [verb,"WithFalse"]            = compileCQuery [verb,"*","WithFalse"]
compileCQuery [verb,"WithNil"]              = compileCQuery [verb,"*","WithNil"]
compileCQuery [verb,"WithTrue"]             = compileCQuery [verb,"*","WithTrue"]
compileCQuery (verb:"WithChar":args)        = compileCQuery (verb:"*":"WithChar":args)
compileCQuery (verb:"WithNumber":args)      = compileCQuery (verb:"*":"WithNumber":args)
compileCQuery (verb:"WithString":args)      = compileCQuery (verb:"*":"WithString":args)
compileCQuery (verb:"WithSymbol":args)      = compileCQuery (verb:"*":"WithSymbol":args)
compileCQuery (verb:object:args)            = Inspection verb (compileBinding object) (compileMatcher args)

compileBinding :: String -> Predicate
compileBinding "*"        = Any
compileBinding ('~':name) = Like name
compileBinding ('=':name) = Named name
compileBinding ('^':name) = Except name
compileBinding ('[':ns)   | last ns == ']' = AnyOf . splitOn "|" . init $ ns
compileBinding name       = Named name

compileMatcher :: [String] -> Matcher
compileMatcher = matching . f
  where
    matching [] = Unmatching -- TODO move to explang
    matching xs = Matching xs

    f :: [String] -> [Clause]
    f ("WithFalse":args)        =  IsFalse : f args
    f ("WithNil":args)          =  IsNil : f args
    f ("WithTrue":args)         =  IsTrue : f args
    f ("WithChar":value:args)   =  IsChar (read value) : f args
    f ("WithNumber":value:args) =  IsNumber (read value) : f args
    f ("WithString":value:args) =  IsString (read value) : f args
    f ("WithSymbol":value:args) =  IsSymbol (read ("\""++value++"\"")) : f args
    f []                        = []
