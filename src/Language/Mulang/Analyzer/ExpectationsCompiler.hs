module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (Expectation(..))

import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Modifiers = (Scope, PredicateModifier)
type Scope = (Inspection -> Inspection)
type PredicateModifier = (IdentifierPredicate -> IdentifierPredicate)

compileExpectation :: Expectation -> Inspection
compileExpectation = fromMaybe (\_ -> True) . compileMaybe

compileMaybe :: Expectation -> Maybe Inspection
compileMaybe (Expectation b i) = do
  let inspectionParts = splitOn ":" i
  let negator = compileNegator inspectionParts
  (scope, predicateModifier) <- compileModifiers (splitOn ":" b)
  baseInspection             <- compileBaseInspection predicateModifier inspectionParts
  return . negator . scope $ baseInspection

compileModifiers :: [String] -> Maybe Modifiers
compileModifiers ["*"]                 = Just (id, id)
compileModifiers ["Intransitive",name] = justScopeFor scopedList name
compileModifiers [name]                = justScopeFor transitiveList name
compileModifiers _                     = Nothing

justScopeFor f name = Just (flip f names, andAlso (except (last names)))
  where names = splitOn "." name

compileNegator :: [String] -> Scope
compileNegator ("Not":_) = negative
compileNegator _         = id

compileBaseInspection :: PredicateModifier -> [String] -> Maybe (Inspection)
compileBaseInspection p ("Not":parts)         = compileBaseInspection p parts
compileBaseInspection p [verb]                = compileBaseInspection p [verb, "*"]
compileBaseInspection p [verb, object]        = compileInspectionPrimitive verb (compileObject p object)
compileBaseInspection _ _                     = Nothing

compileObject :: PredicateModifier -> String -> IdentifierPredicate
compileObject p "*"        = p $ anyone
compileObject p ('~':name) = p $ like name
compileObject _ ('=':name) = named name
compileObject _ ('^':name) = except name
compileObject _ ('[':ns)   | last ns == ']' = anyOf . splitOn "|" . init $ ns
compileObject _ name       = named name


compileInspectionPrimitive :: String -> IdentifierPredicate -> Maybe Inspection
compileInspectionPrimitive = f
  where
  f "Assigns"                        = binded assigns
  f "Calls"                          = binded calls
  f "Declares"                       = binded declares
  f "DeclaresAttribute"              = binded declaresAttribute
  f "DeclaresClass"                  = binded declaresClass
  f "DeclaresComputation"            = binded declaresComputation
  f "DeclaresComputationWithArity0"  = binded (declaresComputationWithArity 0)
  f "DeclaresComputationWithArity1"  = binded (declaresComputationWithArity 1)
  f "DeclaresComputationWithArity2"  = binded (declaresComputationWithArity 2)
  f "DeclaresComputationWithArity3"  = binded (declaresComputationWithArity 3)
  f "DeclaresComputationWithArity4"  = binded (declaresComputationWithArity 4)
  f "DeclaresComputationWithArity5"  = binded (declaresComputationWithArity 5)
  f "DeclaresEntryPoint"             = binded declaresEntryPoint
  f "DeclaresEnumeration"            = binded declaresEnumeration
  f "DeclaresFact"                   = binded declaresFact
  f "DeclaresFunction"               = binded declaresFunction
  f "DeclaresInterface"              = binded declaresInterface
  f "DeclaresMethod"                 = binded declaresMethod
  f "DeclaresObject"                 = binded declaresObject
  f "DeclaresPredicate"              = binded declaresPredicate
  f "DeclaresProcedure"              = binded declaresProcedure
  f "DeclaresRecursively"            = binded declaresRecursively
  f "DeclaresRule"                   = binded declaresRule
  f "DeclaresSuperclass"             = binded declaresSuperclass
  f "DeclaresTypeAlias"              = binded declaresTypeAlias
  f "DeclaresTypeSignature"          = binded declaresTypeSignature
  f "DeclaresVariable"               = binded declaresVariable
  f "Implements"                     = binded implements
  f "Includes"                       = binded includes
  f "Inherits"                       = binded inherits
  f "Instantiates"                   = binded instantiates
  f "Raises"                         = binded raises
  f "Rescues"                        = binded rescues
  f "TypesParameterAs"               = binded typesParameterAs
  f "TypesAs"                        = binded typesAs
  f "TypesReturnAs"                  = binded typesReturnAs
  f "Uses"                           = binded uses
  f "UsesAnonymousVariable"          = simple usesAnonymousVariable
  f "UsesComposition"                = simple usesComposition
  f "UsesComprehension"              = f "UsesForComprehension"
  f "UsesConditional"                = simple usesConditional
  f "UsesDyamicPolymorphism"         = simple usesDyamicPolymorphism
  f "UsesDynamicMethodOverload"      = simple usesDynamicMethodOverload
  f "UsesExceptionHandling"          = simple usesExceptionHandling
  f "UsesExceptions"                 = simple usesExceptions
  f "UsesFindall"                    = simple usesFindall
  f "UsesFor"                        = simple usesFor
  f "UsesForall"                     = simple usesForall
  f "UsesForComprehension"           = simple usesForComprehension
  f "UsesForeach"                    = simple usesForEach
  f "UsesForLoop"                    = simple usesForLoop
  f "UsesGuards"                     = simple usesGuards
  f "UsesIf"                         = simple usesIf
  f "UsesInheritance"                = simple usesInheritance
  f "UsesLambda"                     = simple usesLambda
  f "UsesMixins"                     = simple usesMixins
  f "UsesNot"                        = simple usesNot
  f "UsesObjectComposition"          = simple usesObjectComposition
  f "UsesPatternMatching"            = simple usesPatternMatching
  f "UsesRepeat"                     = simple usesRepeat
  f "UsesStaticMethodOverload"       = simple usesStaticMethodOverload
  f "UsesStaticPolymorphism"         = simple usesStaticPolymorphism
  f "UsesSwitch"                     = simple usesSwitch
  f "UsesTemplateMethod"             = simple usesTemplateMethod
  f "UsesType"                       = binded usesType
  f "UsesWhile"                      = simple usesWhile
  f "UsesYield"                      = simple usesYield
  f _                                = const Nothing

  simple i _ = Just i
  binded i b = Just $ i b

