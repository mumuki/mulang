{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExplangExpectationsCompiler(
  compileExpectation) where

import Data.Function.Extra (orElse, andAlso, never)

import Language.Mulang
import Language.Mulang.Counter (Counter, plus, atLeast, atMost, exactly)
import Language.Mulang.Inspector.Literal (isNil, isNumber, isBool, isChar, isString, isSymbol)
import Language.Mulang.Analyzer.Synthesizer (decodeUsageInspection, decodeDeclarationInspection)

import qualified Language.Explang.Expectation as E

import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Scope = (ContextualizedInspection -> ContextualizedInspection, IdentifierPredicate -> IdentifierPredicate)

compileExpectation :: E.Expectation -> Inspection
compileExpectation = fromMaybe (\_ -> True) . compileQuery

compileQuery :: E.Query -> Maybe Inspection
compileQuery (E.Decontextualize query) = compileCQuery id query >>= (return . decontextualize)
compileQuery (E.Within name query)     | (scope, p) <- compileWithin name  = fmap (decontextualize.scope) (compileCQuery p query)
compileQuery (E.Through name query)    | (scope, p) <- compileThrough name = fmap (decontextualize.scope) (compileCQuery p query)
compileQuery (E.Not query)             = fmap never (compileQuery query)
compileQuery (E.And query1 query2)     = andAlso <$> (compileQuery query1) <*> (compileQuery query2)
compileQuery (E.Or query1 query2)      = orElse <$> (compileQuery query1) <*> (compileQuery query2)

compileWithin, compileThrough :: String -> Scope
compileWithin name  = scopeFor scopedList name
compileThrough name = scopeFor transitiveList name

scopeFor :: ([Identifier] -> Inspection -> Inspection) -> Identifier -> Scope
scopeFor f name = (contextualized (f names), andAlso (except (last names)))
  where names = splitOn "." name

compileCQuery :: (IdentifierPredicate -> IdentifierPredicate) -> E.CQuery -> Maybe ContextualizedInspection
compileCQuery pm (E.Inspection i p m) = ($ (compilePredicate pm p)) <$> compileInspection i m
compileCQuery pm (E.CNot q)           = contextualized never        <$> compileCQuery pm q
compileCQuery pm (E.CAnd q1 q2)       = contextualized2 andAlso     <$> compileCQuery pm q1 <*> compileCQuery pm q2
compileCQuery pm (E.COr q1 q2)        = contextualized2 orElse      <$> compileCQuery pm q1 <*> compileCQuery pm q2
compileCQuery pm (E.AtLeast n q)      = atLeast n                   <$> compileTQuery pm q
compileCQuery pm (E.AtMost n q)       = atMost n                    <$> compileTQuery pm q
compileCQuery pm (E.Exactly n q)      = exactly n                   <$> compileTQuery pm q

compileTQuery :: (IdentifierPredicate -> IdentifierPredicate) -> E.TQuery -> Maybe Counter
compileTQuery pm (E.Counter i p m) = ($ (compilePredicate pm p)) <$> compileCounter i m
compileTQuery pm (E.Plus q1 q2)    = plus                        <$> (compileTQuery pm q1) <*> (compileTQuery pm q2)

compilePredicate :: (IdentifierPredicate -> IdentifierPredicate) -> E.Predicate -> IdentifierPredicate
compilePredicate p E.Any           = p $ anyone
compilePredicate p (E.Like name)   = p $ like name
compilePredicate _ (E.Named name)  = named name
compilePredicate _ (E.Except name) = except name
compilePredicate _ (E.AnyOf ns)    = anyOf ns


compileCounter :: String -> E.Matcher -> Maybe (IdentifierPredicate -> Counter)
compileCounter = f
  where
  f "UsesIf" = undefined

compileInspection :: String -> E.Matcher -> Maybe ContextualizedBoundInspection
compileInspection = f
  where
  f "Assigns"                          E.Unmatching   = bound assigns
  f "Assigns"                          (E.Matching p) = bound (assignsMatching (compileMatcher p))
  f "Calls"                            E.Unmatching   = bound calls
  f "Calls"                            (E.Matching p) = bound (callsMatching (compileMatcher p))
  f "Declares"                         E.Unmatching   = bound declares
  f "DeclaresAttribute"                E.Unmatching   = bound declaresAttribute
  f "DeclaresClass"                    E.Unmatching   = bound declaresClass
  f "DeclaresComputation"              E.Unmatching   = bound declaresComputation
  f "DeclaresComputationWithArity0"    E.Unmatching   = bound (declaresComputationWithArity 0)
  f "DeclaresComputationWithArity1"    E.Unmatching   = bound (declaresComputationWithArity 1)
  f "DeclaresComputationWithArity2"    E.Unmatching   = bound (declaresComputationWithArity 2)
  f "DeclaresComputationWithArity3"    E.Unmatching   = bound (declaresComputationWithArity 3)
  f "DeclaresComputationWithArity4"    E.Unmatching   = bound (declaresComputationWithArity 4)
  f "DeclaresComputationWithArity5"    E.Unmatching   = bound (declaresComputationWithArity 5)
  f "DeclaresEntryPoint"               E.Unmatching   = bound declaresEntryPoint
  f "DeclaresEnumeration"              E.Unmatching   = bound declaresEnumeration
  f "DeclaresFact"                     E.Unmatching   = bound declaresFact
  f "DeclaresFunction"                 E.Unmatching   = bound declaresFunction
  f "DeclaresInterface"                E.Unmatching   = bound declaresInterface
  f "DeclaresMethod"                   E.Unmatching   = bound declaresMethod
  f "DeclaresObject"                   E.Unmatching   = bound declaresObject
  f "DeclaresPredicate"                E.Unmatching   = bound declaresPredicate
  f "DeclaresProcedure"                E.Unmatching   = bound declaresProcedure
  f "DeclaresRecursively"              E.Unmatching   = bound declaresRecursively
  f "DeclaresRule"                     E.Unmatching   = bound declaresRule
  f "DeclaresSuperclass"               E.Unmatching   = bound declaresSuperclass
  f "DeclaresTypeAlias"                E.Unmatching   = bound declaresTypeAlias
  f "DeclaresTypeSignature"            E.Unmatching   = bound declaresTypeSignature
  f "DeclaresVariable"                 E.Unmatching   = bound declaresVariable
  f "Delegates"                        E.Unmatching   = contextualizedBound delegates'
  f "Implements"                       E.Unmatching   = bound implements
  f "Includes"                         E.Unmatching   = bound includes
  f "Inherits"                         E.Unmatching   = bound inherits
  f "Instantiates"                     E.Unmatching   = bound instantiates
  f "Raises"                           E.Unmatching   = bound raises
  f "Rescues"                          E.Unmatching   = bound rescues
  f "Returns"                          (E.Matching p) = plain (returnsMatching (compileMatcher p))
  f "TypesAs"                          E.Unmatching   = bound typesAs
  f "TypesParameterAs"                 E.Unmatching   = bound typesParameterAs
  f "TypesReturnAs"                    E.Unmatching   = bound typesReturnAs
  f "Uses"                             E.Unmatching   = bound uses
  f "UsesAnonymousVariable"            E.Unmatching   = plain usesAnonymousVariable
  f "UsesBooleanLogic"                 E.Unmatching   = plain usesBooleanLogic
  f "UsesArithmetic"                   E.Unmatching   = plain usesArithmetic
  f "UsesComposition"                  E.Unmatching   = plain usesComposition
  f "UsesComprehension"                E.Unmatching   = f "UsesForComprehension" E.Unmatching
  f "UsesConditional"                  E.Unmatching   = plain usesConditional
  f "UsesDyamicPolymorphism"           E.Unmatching   = contextualized usesDyamicPolymorphism'
  f "UsesDynamicMethodOverload"        E.Unmatching   = plain usesDynamicMethodOverload
  f "UsesExceptionHandling"            E.Unmatching   = plain usesExceptionHandling
  f "UsesExceptions"                   E.Unmatching   = plain usesExceptions
  f "UsesFindall"                      E.Unmatching   = plain usesFindall
  f "UsesFor"                          E.Unmatching   = plain usesFor
  f "UsesForall"                       E.Unmatching   = plain usesForall
  f "UsesForComprehension"             E.Unmatching   = plain usesForComprehension
  f "UsesForeach"                      E.Unmatching   = plain usesForEach
  f "UsesForLoop"                      E.Unmatching   = plain usesForLoop
  f "UsesGuards"                       E.Unmatching   = plain usesGuards
  f "UsesIf"                           E.Unmatching   = plain usesIf
  f "UsesInheritance"                  E.Unmatching   = plain usesInheritance
  f "UsesLambda"                       E.Unmatching   = plain usesLambda
  f "UsesLoop"                         E.Unmatching   = plain usesLoop
  f "UsesMixins"                       E.Unmatching   = plain usesMixins
  f "UsesNot"                          E.Unmatching   = plain usesNot
  f "UsesObjectComposition"            E.Unmatching   = plain usesObjectComposition
  f "UsesPatternMatching"              E.Unmatching   = plain usesPatternMatching
  f "UsesPrint"                        E.Unmatching   = plain usesPrint
  f "UsesRepeat"                       E.Unmatching   = plain usesRepeat
  f "UsesStaticMethodOverload"         E.Unmatching   = plain usesStaticMethodOverload
  f "UsesStaticPolymorphism"           E.Unmatching   = contextualized usesStaticPolymorphism'
  f "UsesSwitch"                       E.Unmatching   = plain usesSwitch
  f "UsesTemplateMethod"               E.Unmatching   = plain usesTemplateMethod
  f "UsesType"                         E.Unmatching   = bound usesType
  f "UsesWhile"                        E.Unmatching   = plain usesWhile
  f "UsesYield"                        E.Unmatching   = plain usesYield
  f (primitiveUsage -> Just p)         E.Unmatching   = plain (usesPrimitive p)
  f (primitiveDeclaration -> Just p)   E.Unmatching   = plain (declaresPrimitive p)
  f _                                  _              = Nothing

  primitiveUsage = decodeUsageInspection
  primitiveDeclaration = decodeDeclarationInspection

  contextualized :: ContextualizedInspection -> Maybe ContextualizedBoundInspection
  contextualized = Just . contextualizedBind

  contextualizedBound :: ContextualizedBoundInspection -> Maybe ContextualizedBoundInspection
  contextualizedBound = Just

  plain :: Inspection -> Maybe ContextualizedBoundInspection
  plain = Just . contextualizedBind . contextualize

  bound :: BoundInspection -> Maybe ContextualizedBoundInspection
  bound = Just . boundContextualize

compileMatcher :: [E.Clause] -> Matcher
compileMatcher = withEvery . f
  where
    f :: [E.Clause] -> [Inspection]
    f (E.IsFalse:args)         = isBool False : (f args)
    f (E.IsNil:args)           = isNil : (f args)
    f (E.IsTrue:args)          = isBool True : (f args)
    f (E.IsChar value:args)    = isChar value : (f args)
    f (E.IsNumber value:args)  = isNumber value : (f args)
    f (E.IsString value:args)  = isString value : (f args)
    f (E.IsSymbol value:args)  = isSymbol value : (f args)
    f []                     = []
