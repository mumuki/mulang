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

  f "Assigns"                             b = Just $ assigns b
  f "Declares"                            b = Just $ declares b
  f "DeclaresAttribute"                   b = Just $ declaresAttribute b
  f "DeclaresClass"                       b = Just $ declaresClass b
  f "DeclaresComputation"                 b = Just $ declaresComputation b
  f "DeclaresComputationWithArity0"       b = Just $ declaresComputationWithArity 0 b
  f "DeclaresComputationWithArity1"       b = Just $ declaresComputationWithArity 1 b
  f "DeclaresComputationWithArity2"       b = Just $ declaresComputationWithArity 2 b
  f "DeclaresComputationWithArity3"       b = Just $ declaresComputationWithArity 3 b
  f "DeclaresComputationWithArity4"       b = Just $ declaresComputationWithArity 4 b
  f "DeclaresComputationWithArity5"       b = Just $ declaresComputationWithArity 5 b
  f "DeclaresEntryPoint"                  _ = Just declaresEntryPoint
  f "DeclaresFact"                        b = Just $ declaresFact b
  f "DeclaresFunction"                    b = Just $ declaresFunction b
  f "DeclaresMethod"                      b = Just $ declaresMethod b
  f "DeclaresObject"                      b = Just $ declaresObject b
  f "DeclaresPredicate"                   b = Just $ declaresPredicate b
  f "DeclaresProcedure"                   b = Just $ declaresProcedure b
  f "DeclaresRecursively"                 b = Just $ declaresRecursively b
  f "DeclaresRule"                        b = Just $ declaresRule b
  f "DeclaresSuperclass"                  b = Just $ declaresSuperclass b
  f "DeclaresTypeAlias"                   b = Just $ declaresTypeAlias b
  f "DeclaresTypeSignature"               b = Just $ declaresTypeSignature b
  f "DeclaresVariable"                    b = Just $ declaresVariable b
  f "Uses"                                b = Just $ uses b
  f "UsesAnonymousVariable"               _ = Just usesAnonymousVariable
  f "UsesComposition"                     _ = Just usesComposition
  f "UsesComprehension"                   _ = Just usesComprehension
  f "UsesConditional"                     _ = Just usesConditional
  f "UsesFindall"                         _ = Just usesFindall
  f "UsesForall"                          _ = Just usesForall
  f "UsesGuards"                          _ = Just usesGuards
  f "UsesIf"                              _ = Just usesIf
  f "UsesInheritance"                     _ = Just usesInheritance
  f "UsesLambda"                          _ = Just usesLambda
  f "UsesNot"                             _ = Just usesNot
  f "UsesPatternMatching"                 _ = Just usesPatternMatching
  f "UsesRepeat"                          _ = Just usesRepeat
  f "UsesSwitch"                          _ = Just usesSwitch
  f "UsesWhile"                           _ = Just usesWhile
  f _                                     _ = Nothing

