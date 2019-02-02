module Language.Mulang.Inspector.Primitive (
  anyExpression,
  anyExpressions,
  containsExpression,
  containsDeclaration,
  containsBody,
  matchesType,
  Inspection,
  MultiInspection) where

import           Language.Mulang.Ast
import           Language.Mulang.Identifier (IdentifierPredicate)
import           Language.Mulang.Generator (expressions, equationBodies, declarations)

import           Data.List.Extra (has)

type Inspection = Expression -> Bool
type MultiInspection = [Expression] -> Bool

anyExpression :: Inspection
anyExpression = const True

anyExpressions :: MultiInspection
anyExpressions = const True

containsExpression :: Inspection -> Inspection
containsExpression f = has f expressions

containsBody :: (EquationBody -> Bool)-> Inspection
containsBody f = has f equationBodies

containsDeclaration :: Inspection -> Inspection
containsDeclaration f = has f declarations

matchesType :: IdentifierPredicate -> Pattern -> Bool
matchesType predicate (TypePattern n)               = predicate n
matchesType predicate (AsPattern _ (TypePattern n)) = predicate n
matchesType predicate (UnionPattern patterns)       = any (matchesType predicate) patterns
matchesType _         _                             = False

