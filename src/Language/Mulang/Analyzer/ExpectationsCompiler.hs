module Language.Mulang.Analyzer.ExpectationsCompiler(
  compileExpectation) where

import Language.Mulang
import Language.Mulang.Analyzer.Analysis (Expectation(..))

import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Slicer = (Inspection -> Inspection)
type Predicator = (BindingPredicate -> BindingPredicate)

compileExpectation :: Expectation -> Inspection
compileExpectation = fromMaybe (\_ -> True) . compileMaybe

compileMaybe :: Expectation -> Maybe Inspection
compileMaybe (Expectation b i) = do
  let inspectionParts = splitOn ":" i
  let negator = compileNegator inspectionParts
  (slicer, predicator) <- compileSlicer (splitOn ":" b)
  baseInspection       <- compileBaseInspection predicator inspectionParts
  return . negator . slicer $ baseInspection

compileSlicer :: [String] -> Maybe (Slicer, Predicator)
compileSlicer [""]                  = Just (id, id)
compileSlicer ["Intransitive",name] = justSlicerFor scopedList name
compileSlicer [name]                = justSlicerFor transitiveList name
compileSlicer _                     = Nothing

justSlicerFor f name = Just (flip f names, andAlso (except (last names)))
  where names = splitOn "." name

compileNegator :: [String] -> Slicer
compileNegator ("Not":_) = negative
compileNegator _         = id

compileBaseInspection :: Predicator -> [String] -> Maybe (Inspection)
compileBaseInspection p ("Not":parts)         = compileBaseInspection p parts
compileBaseInspection p [verb]                = compileBaseInspection p [verb, "*"]
compileBaseInspection p [verb, object]        = compileInspectionPrimitive verb (compileObject p object)
compileBaseInspection _ _                     = Nothing

compileObject :: Predicator -> String -> BindingPredicate
compileObject p "*"        = p $ anyone
compileObject p ('~':name) = p $ like name
compileObject _ ('=':name) = named name
compileObject _ ('^':name) = except name
compileObject _ name       = named name


compileInspectionPrimitive :: String -> BindingPredicate -> Maybe Inspection
compileInspectionPrimitive = f
  where

  f "Assigns"                        = binded assigns
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
  f "Uses"                           = binded uses
  f "UsesAnonymousVariable"          = simple usesAnonymousVariable
  f "UsesComposition"                = simple usesComposition
  f "UsesComprehension"              = simple usesComprehension
  f "UsesConditional"                = simple usesConditional
  f "UsesFindall"                    = simple usesFindall
  f "UsesForall"                     = simple usesForall
  f "UsesGuards"                     = simple usesGuards
  f "UsesIf"                         = simple usesIf
  f "UsesInheritance"                = simple usesInheritance
  f "UsesLambda"                     = simple usesLambda
  f "UsesMixins"                     = simple usesMixins
  f "UsesNot"                        = simple usesNot
  f "UsesPatternMatching"            = simple usesPatternMatching
  f "UsesRepeat"                     = simple usesRepeat
  f "UsesSwitch"                     = simple usesSwitch
  f "UsesWhile"                      = simple usesWhile
  f _                                = const Nothing

  simple i _ = Just i
  binded i b = Just $ i b
