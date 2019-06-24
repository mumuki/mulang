module Language.Mulang.Inspector.Primitive (
  containsExpression,
  containsDeclaration,
  containsBody,
  matchesType,
  countExpressions,
  positive,
  atLeast,
  atMost,
  exactly,
  Counter,
  Inspection) where

import Data.Count (Count, counts)
import Data.List.Extra (has)

import Language.Mulang.Ast
import Language.Mulang.Consult (Consult)
import Language.Mulang.Counter (Counter)
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Generator (expressions, equationBodies, declarations)

type Inspection = Consult Bool

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

countExpressions :: Inspection -> Counter
countExpressions f = counts f expressions

positive :: Counter -> Inspection
positive = atLeast 1

atLeast :: Count -> Counter -> Inspection
atLeast n = (.) (>= n)

atMost :: Count -> Counter -> Inspection
atMost n = (.) (<= n)

exactly :: Count -> Counter -> Inspection
exactly n = (.) (== n)
