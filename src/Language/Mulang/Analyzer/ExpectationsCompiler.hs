module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (Expectation(..))

import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Modifiers = (ContextualizedModifier, PredicateModifier)

compileExpectation :: Expectation -> Inspection
compileExpectation = fromMaybe (\_ -> True) . compileMaybe

compileMaybe :: Expectation -> Maybe Inspection
compileMaybe (Expectation s i) = do
  let inspectionParts = splitOn ":" i
  let negator = compileNegator inspectionParts
  (scope, predicateModifier) <- compileModifiers (splitOn ":" s)
  baseInspection             <- compileBaseInspection predicateModifier inspectionParts
  return . negator . decontextualize . scope  $ baseInspection

compileModifiers :: [String] -> Maybe Modifiers
compileModifiers ["*"]                 = Just (id, id)
compileModifiers ["Intransitive",name] = justScopeFor contextualizedScopedList name
compileModifiers [name]                = justScopeFor contextualizedTransitiveList name
compileModifiers _                     = Nothing

justScopeFor :: ([Identifier] -> ContextualizedModifier) -> Identifier -> Maybe Modifiers
justScopeFor f name = Just (f names, andAlso (except (last names)))
  where names = splitOn "." name

compileNegator :: [String] -> Modifier
compileNegator ("Not":_) = negative
compileNegator _         = id

compileBaseInspection :: PredicateModifier -> [String] -> Maybe ContextualizedInspection
compileBaseInspection p ("Not":parts)                 = compileBaseInspection p parts
compileBaseInspection p [verb]                        = compileBaseInspection p [verb, "*"]
compileBaseInspection p [verb, object]                = compileBaseInspection p [verb, object, "With"]
compileBaseInspection p (verb:"With":args)            = compileBaseInspection p (verb:"*":"With":args)
compileBaseInspection p (verb:object:"With":args)     = fmap ($ (compileObject p object)) (compileInspectionPrimitive (verb:args))
compileBaseInspection _ _                             = Nothing

compileObject :: PredicateModifier -> String -> IdentifierPredicate
compileObject p "*"        = p $ anyone
compileObject p ('~':name) = p $ like name
compileObject _ ('=':name) = named name
compileObject _ ('^':name) = except name
compileObject _ ('[':ns)   | last ns == ']' = anyOf . splitOn "|" . init $ ns
compileObject _ name       = named name

compileInspectionPrimitive :: [String] -> Maybe ContextualizedBoundInspection
compileInspectionPrimitive = f
  where
  f ["Assigns"]                        = bound assigns
  f ["Assigns", value]                 = bound (assignsMatching (that (isLiteral value)))
  f ["Calls"]                          = bound calls
  f ("Calls":args)                     = bound (callsMatching (thatEvery (map isLiteral args)))
  f ["Declares"]                       = bound declares
  f ["DeclaresAttribute"]              = bound declaresAttribute
  f ["DeclaresClass"]                  = bound declaresClass
  f ["DeclaresComputation"]            = bound declaresComputation
  f ["DeclaresComputationWithArity0"]  = bound (declaresComputationWithArity 0)
  f ["DeclaresComputationWithArity1"]  = bound (declaresComputationWithArity 1)
  f ["DeclaresComputationWithArity2"]  = bound (declaresComputationWithArity 2)
  f ["DeclaresComputationWithArity3"]  = bound (declaresComputationWithArity 3)
  f ["DeclaresComputationWithArity4"]  = bound (declaresComputationWithArity 4)
  f ["DeclaresComputationWithArity5"]  = bound (declaresComputationWithArity 5)
  f ["DeclaresEntryPoint"]             = bound declaresEntryPoint
  f ["DeclaresEnumeration"]            = bound declaresEnumeration
  f ["DeclaresFact"]                   = bound declaresFact
  f ["DeclaresFunction"]               = bound declaresFunction
  f ["DeclaresInterface"]              = bound declaresInterface
  f ["DeclaresMethod"]                 = bound declaresMethod
  f ["DeclaresObject"]                 = bound declaresObject
  f ["DeclaresPredicate"]              = bound declaresPredicate
  f ["DeclaresProcedure"]              = bound declaresProcedure
  f ["DeclaresRecursively"]            = bound declaresRecursively
  f ["DeclaresRule"]                   = bound declaresRule
  f ["DeclaresSuperclass"]             = bound declaresSuperclass
  f ["DeclaresTypeAlias"]              = bound declaresTypeAlias
  f ["DeclaresTypeSignature"]          = bound declaresTypeSignature
  f ["DeclaresVariable"]               = bound declaresVariable
  f ["Delegates"]                      = contextualizedBound delegates'
  f ["Implements"]                     = bound implements
  f ["Includes"]                       = bound includes
  f ["Inherits"]                       = bound inherits
  f ["Instantiates"]                   = bound instantiates
  f ["Raises"]                         = bound raises
  f ["Rescues"]                        = bound rescues
  f ["TypesAs"]                        = bound typesAs
  f ["TypesParameterAs"]               = bound typesParameterAs
  f ["TypesReturnAs"]                  = bound typesReturnAs
  f ["Returns", value]                 = plain (returnsMatching (that (isLiteral value)))
  f ["Uses"]                           = bound uses
  f ["UsesAnonymousVariable"]          = plain usesAnonymousVariable
  f ["UsesComposition"]                = plain usesComposition
  f ["UsesComprehension"]              = f ["UsesForComprehension"]
  f ["UsesConditional"]                = plain usesConditional
  f ["UsesDyamicPolymorphism"]         = contextualized usesDyamicPolymorphism'
  f ["UsesDynamicMethodOverload"]      = plain usesDynamicMethodOverload
  f ["UsesExceptionHandling"]          = plain usesExceptionHandling
  f ["UsesExceptions"]                 = plain usesExceptions
  f ["UsesFindall"]                    = plain usesFindall
  f ["UsesFor"]                        = plain usesFor
  f ["UsesForall"]                     = plain usesForall
  f ["UsesForComprehension"]           = plain usesForComprehension
  f ["UsesForeach"]                    = plain usesForEach
  f ["UsesForLoop"]                    = plain usesForLoop
  f ["UsesGuards"]                     = plain usesGuards
  f ["UsesIf"]                         = plain usesIf
  f ["UsesInheritance"]                = plain usesInheritance
  f ["UsesLambda"]                     = plain usesLambda
  f ["UsesLoop"]                       = plain usesLoop
  f ["UsesMixins"]                     = plain usesMixins
  f ["UsesNot"]                        = plain usesNot
  f ["UsesObjectComposition"]          = plain usesObjectComposition
  f ["UsesPatternMatching"]            = plain usesPatternMatching
  f ["UsesRepeat"]                     = plain usesRepeat
  f ["UsesStaticMethodOverload"]       = plain usesStaticMethodOverload
  f ["UsesStaticPolymorphism"]         = contextualized usesStaticPolymorphism'
  f ["UsesSwitch"]                     = plain usesSwitch
  f ["UsesTemplateMethod"]             = plain usesTemplateMethod
  f ["UsesType"]                       = bound usesType
  f ["UsesWhile"]                      = plain usesWhile
  f ["UsesYield"]                      = plain usesYield
  f _                                = Nothing

  contextualized :: ContextualizedInspection -> Maybe ContextualizedBoundInspection
  contextualized = Just . contextualizedBind

  contextualizedBound :: ContextualizedBoundInspection -> Maybe ContextualizedBoundInspection
  contextualizedBound = Just

  plain :: Inspection -> Maybe ContextualizedBoundInspection
  plain = Just . contextualizedBind . contextualize

  bound :: BoundInspection -> Maybe ContextualizedBoundInspection
  bound = Just . boundContextualize
