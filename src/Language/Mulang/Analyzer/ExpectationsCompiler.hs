{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import           Language.Mulang.Edl.Expectation

import           Language.Mulang (Inspection)
import           Language.Mulang.Analyzer.EdlQueryCompiler (compileTopQuery)
import qualified Language.Mulang.Analyzer.Analysis as A

import Data.List.Split (splitOn)

compileExpectation :: A.Expectation -> Inspection
compileExpectation (A.Expectation s i) = compileTopQuery . negator . scope $ baseQuery
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
compileBinding "*"          = Any
compileBinding ('^':'~':'[':ns) | last ns == ']' = compileBindings LikeNoneOf ns
compileBinding ('~':'[':ns) | last ns == ']'     = compileBindings LikeAnyOf ns
compileBinding ('^':'[':ns) | last ns == ']'     = compileBindings NoneOf ns
compileBinding ('[':ns)     | last ns == ']'     = compileBindings AnyOf ns
compileBinding ('^':'~':name)                    = NotLike name
compileBinding ('^':name)                        = Except name
compileBinding ('~':name)                        = Like name
compileBinding ('=':name)                        = Named name
compileBinding name                              = Named name

compileBindings f = f . splitOn "|" . init

compileMatcher :: [String] -> Matcher
compileMatcher = matching . f
  where
    matching [] = Unmatching
    matching xs = Matching xs

    f :: [String] -> [Clause]
    f ("WithFalse":args)        =  IsFalse : f args
    f ("WithLiteral":args)      =  IsLiteral : f args
    f ("WithLogic":args)        =  IsLogic   : f args
    f ("WithMath":args)         =  IsMath : f args
    f ("WithNil":args)          =  IsNil : f args
    f ("WithNonliteral":args)   =  IsNonliteral : f args
    f ("WithTrue":args)         =  IsTrue : f args
    f ("WithChar":value:args)   =  IsChar (read value) : f args
    f ("WithNumber":value:args) =  IsNumber (read value) : f args
    f ("WithString":value:args) =  IsString (read value) : f args
    f ("WithSymbol":value:args) =  IsSymbol (read ("\""++value++"\"")) : f args
    f []                        = []
