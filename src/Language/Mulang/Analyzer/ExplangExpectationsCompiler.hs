{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.ExplangExpectationsCompiler(
  compileExpectation) where

import Language.Mulang hiding (Query)
import Language.Mulang.Inspector.Literal (isNil, isNumber, isBool, isChar, isString, isSymbol)
import Language.Mulang.Analyzer.Synthesizer (decodeUsageInspection, decodeDeclarationInspection)

import Language.Explang.Expectation hiding (Matcher)
import qualified Language.Explang.Expectation as E


import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Modifiers = (ContextualizedModifier, PredicateModifier)

compileExpectation :: Expectation -> Inspection
compileExpectation = fromMaybe (\_ -> True) . compileMaybe

compileMaybe :: Expectation -> Maybe Inspection
compileMaybe (Expectation flags scope query count) = do
  (scope, predicateModifier) <- compileModifiers flags scope
  baseInspection <- compileBaseInspection predicateModifier query
  return . decontextualize . scope  $ baseInspection

compileModifiers :: Flags -> Scope -> Maybe Modifiers
compileModifiers (Flags False) Unscoped      = Just (id, id)
compileModifiers (Flags True)  (Scoped name) = justScopeFor contextualizedScopedList name
compileModifiers (Flags False) (Scoped name) = justScopeFor contextualizedTransitiveList name
compileModifiers _             _             = Nothing

justScopeFor :: ([Identifier] -> ContextualizedModifier) -> Identifier -> Maybe Modifiers
justScopeFor f name = Just (f names, andAlso (except (last names)))
  where names = splitOn "." name

compileNegator :: [String] -> Modifier
compileNegator ("Not":_) = negative
compileNegator _         = id

compileBaseInspection :: PredicateModifier -> Query -> Maybe ContextualizedInspection
compileBaseInspection p (E.Inspection i b m) = fmap ($ (compileObject p b)) (compileInspectionPrimitive i m)
compileBaseInspection _ _                    = Nothing

compileObject :: PredicateModifier -> Binding -> IdentifierPredicate
compileObject p Any           = p $ anyone
compileObject p (Like name)   = p $ like name
compileObject _ (Named name)  = named name
compileObject _ (Except name) = except name
compileObject _ (AnyOf ns)    = anyOf ns

compileInspectionPrimitive :: String -> E.Matcher -> Maybe ContextualizedBoundInspection
compileInspectionPrimitive = f
  where
  f "Assigns"                          Unmatching   = bound assigns
  f "Assigns"                          (Matching p) = bound (assignsMatching (compileMatcher p))
  f "Calls"                            Unmatching   = bound calls
  f "Calls"                            (Matching p) = bound (callsMatching (compileMatcher p))
  f "Declares"                         Unmatching   = bound declares
  f "DeclaresAttribute"                Unmatching   = bound declaresAttribute
  f "DeclaresClass"                    Unmatching   = bound declaresClass
  f "DeclaresComputation"              Unmatching   = bound declaresComputation
  f "DeclaresComputationWithArity0"    Unmatching   = bound (declaresComputationWithArity 0)
  f "DeclaresComputationWithArity1"    Unmatching   = bound (declaresComputationWithArity 1)
  f "DeclaresComputationWithArity2"    Unmatching   = bound (declaresComputationWithArity 2)
  f "DeclaresComputationWithArity3"    Unmatching   = bound (declaresComputationWithArity 3)
  f "DeclaresComputationWithArity4"    Unmatching   = bound (declaresComputationWithArity 4)
  f "DeclaresComputationWithArity5"    Unmatching   = bound (declaresComputationWithArity 5)
  f "DeclaresEntryPoint"               Unmatching   = bound declaresEntryPoint
  f "DeclaresEnumeration"              Unmatching   = bound declaresEnumeration
  f "DeclaresFact"                     Unmatching   = bound declaresFact
  f "DeclaresFunction"                 Unmatching   = bound declaresFunction
  f "DeclaresInterface"                Unmatching   = bound declaresInterface
  f "DeclaresMethod"                   Unmatching   = bound declaresMethod
  f "DeclaresObject"                   Unmatching   = bound declaresObject
  f "DeclaresPredicate"                Unmatching   = bound declaresPredicate
  f "DeclaresProcedure"                Unmatching   = bound declaresProcedure
  f "DeclaresRecursively"              Unmatching   = bound declaresRecursively
  f "DeclaresRule"                     Unmatching   = bound declaresRule
  f "DeclaresSuperclass"               Unmatching   = bound declaresSuperclass
  f "DeclaresTypeAlias"                Unmatching   = bound declaresTypeAlias
  f "DeclaresTypeSignature"            Unmatching   = bound declaresTypeSignature
  f "DeclaresVariable"                 Unmatching   = bound declaresVariable
  f "Delegates"                        Unmatching   = contextualizedBound delegates'
  f "Implements"                       Unmatching   = bound implements
  f "Includes"                         Unmatching   = bound includes
  f "Inherits"                         Unmatching   = bound inherits
  f "Instantiates"                     Unmatching   = bound instantiates
  f "Raises"                           Unmatching   = bound raises
  f "Rescues"                          Unmatching   = bound rescues
  f "Returns"                          (Matching p) = plain (returnsMatching (compileMatcher p))
  f "TypesAs"                          Unmatching   = bound typesAs
  f "TypesParameterAs"                 Unmatching   = bound typesParameterAs
  f "TypesReturnAs"                    Unmatching   = bound typesReturnAs
  f "Uses"                             Unmatching   = bound uses
  f "UsesAnonymousVariable"            Unmatching   = plain usesAnonymousVariable
  f "UsesBooleanLogic"                 Unmatching   = plain usesBooleanLogic
  f "UsesArithmetic"                   Unmatching   = plain usesArithmetic
  f "UsesComposition"                  Unmatching   = plain usesComposition
  f "UsesComprehension"                Unmatching   = f "UsesForComprehension" Unmatching
  f "UsesConditional"                  Unmatching   = plain usesConditional
  f "UsesDyamicPolymorphism"           Unmatching   = contextualized usesDyamicPolymorphism'
  f "UsesDynamicMethodOverload"        Unmatching   = plain usesDynamicMethodOverload
  f "UsesExceptionHandling"            Unmatching   = plain usesExceptionHandling
  f "UsesExceptions"                   Unmatching   = plain usesExceptions
  f "UsesFindall"                      Unmatching   = plain usesFindall
  f "UsesFor"                          Unmatching   = plain usesFor
  f "UsesForall"                       Unmatching   = plain usesForall
  f "UsesForComprehension"             Unmatching   = plain usesForComprehension
  f "UsesForeach"                      Unmatching   = plain usesForEach
  f "UsesForLoop"                      Unmatching   = plain usesForLoop
  f "UsesGuards"                       Unmatching   = plain usesGuards
  f "UsesIf"                           Unmatching   = plain usesIf
  f "UsesInheritance"                  Unmatching   = plain usesInheritance
  f "UsesLambda"                       Unmatching   = plain usesLambda
  f "UsesLoop"                         Unmatching   = plain usesLoop
  f "UsesMixins"                       Unmatching   = plain usesMixins
  f "UsesNot"                          Unmatching   = plain usesNot
  f "UsesObjectComposition"            Unmatching   = plain usesObjectComposition
  f "UsesPatternMatching"              Unmatching   = plain usesPatternMatching
  f "UsesPrint"                        Unmatching   = plain usesPrint
  f "UsesRepeat"                       Unmatching   = plain usesRepeat
  f "UsesStaticMethodOverload"         Unmatching   = plain usesStaticMethodOverload
  f "UsesStaticPolymorphism"           Unmatching   = contextualized usesStaticPolymorphism'
  f "UsesSwitch"                       Unmatching   = plain usesSwitch
  f "UsesTemplateMethod"               Unmatching   = plain usesTemplateMethod
  f "UsesType"                         Unmatching   = bound usesType
  f "UsesWhile"                        Unmatching   = plain usesWhile
  f "UsesYield"                        Unmatching   = plain usesYield
  f (primitiveUsage -> Just p)         Unmatching   = plain (usesPrimitive p)
  f (primitiveDeclaration -> Just p)   Unmatching   = plain (declaresPrimitive p)
  f _                                  Unmatching   = Nothing

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

compileMatcher :: [Predicate] -> Matcher
compileMatcher = withEvery . f
  where
    f :: [Predicate] -> [Inspection]
    f (IsFalse:args)         = isBool False : (f args)
    f (IsNil:args)           = isNil : (f args)
    f (IsTrue:args)          = isBool True : (f args)
    f (IsChar value:args)    = isChar value : (f args)
    f (IsNumber value:args)  = isNumber value : (f args)
    f (IsString value:args)  = isString value : (f args)
    f (IsSymbol value:args)  = isSymbol value : (f args)
    f []                     = []
