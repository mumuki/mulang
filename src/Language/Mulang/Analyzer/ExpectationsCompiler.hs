{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import           Language.Explang.Expectation

import           Language.Mulang (Inspection)
import qualified Language.Mulang.Analyzer.ExplangExpectationsCompiler as EC
import qualified Language.Mulang.Analyzer.Analysis as A

import Data.List.Split (splitOn)

compileExpectation :: A.Expectation -> Inspection
compileExpectation (A.Expectation s i) = EC.compileExpectation . negator . scope $ Expectation noFlags Unscoped baseQuery AnyCount -- TODO move to explang
  where
    (inspectionParts, negator) = compileInspectionPartsAndNegator (splitOn ":" i)
    scope = compileScopeAndFlags (splitOn ":" s)
    baseQuery = compileBaseQuery inspectionParts

compileInspectionPartsAndNegator :: [String] -> ([String], Expectation -> Expectation)
compileInspectionPartsAndNegator ("Not":ps) = (ps, \e -> e { query = Not (query e) })
compileInspectionPartsAndNegator ps         = (ps, id)

compileScopeAndFlags :: [String] -> Expectation -> Expectation
compileScopeAndFlags ["*"]                 e = e { scope = Unscoped }
compileScopeAndFlags ["Intransitive",name] e = e { flags = intransitiveFlag, scope = Scoped name }
compileScopeAndFlags [name]                e = e { scope = Scoped name}
compileScopeAndFlags _                     e = e

compileBaseQuery :: [String] -> Query
compileBaseQuery []                            = compileBaseQuery ["Parses","*"]
compileBaseQuery [verb]                        = compileBaseQuery [verb,"*"]
compileBaseQuery [verb,"WithFalse"]            = compileBaseQuery [verb,"*","WithFalse"]
compileBaseQuery [verb,"WithNil"]              = compileBaseQuery [verb,"*","WithNil"]
compileBaseQuery [verb,"WithTrue"]             = compileBaseQuery [verb,"*","WithTrue"]
compileBaseQuery (verb:"WithChar":args)        = compileBaseQuery (verb:"*":"WithChar":args)
compileBaseQuery (verb:"WithNumber":args)      = compileBaseQuery (verb:"*":"WithNumber":args)
compileBaseQuery (verb:"WithString":args)      = compileBaseQuery (verb:"*":"WithString":args)
compileBaseQuery (verb:"WithSymbol":args)      = compileBaseQuery (verb:"*":"WithSymbol":args)
compileBaseQuery (verb:object:args)            = Inspection verb (compileBinding object) (compileMatcher args)

compileBinding :: String -> Binding
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

    f :: [String] -> [Predicate]
    f ("WithFalse":args)        =  IsFalse : f args
    f ("WithNil":args)          =  IsNil : f args
    f ("WithTrue":args)         =  IsTrue : f args
    f ("WithChar":value:args)   =  IsChar (read value) : f args
    f ("WithNumber":value:args) =  IsNumber (read value) : f args
    f ("WithString":value:args) =  IsString (read value) : f args
    f ("WithSymbol":value:args) =  IsSymbol (read ("\""++value++"\"")) : f args
    f []                        = []
