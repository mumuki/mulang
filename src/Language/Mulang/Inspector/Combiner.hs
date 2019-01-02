module Language.Mulang.Inspector.Combiner (
  detect,
  negative,
  alternative,
  scoped,
  scopedList,
  transitive,
  transitiveList) where

import Language.Mulang.Ast
import Language.Mulang.Generator (transitiveReferencedIdentifiers, declarationsOf, declaredIdentifiers)
import Language.Mulang.Inspector.Primitive

detect :: Inspection -> Expression -> [Identifier]
detect i expression =
  filter (`inspection` expression) $ declaredIdentifiers expression
    where inspection = scoped i

alternative :: Inspection -> Inspection -> Inspection
alternative i1 i2 expression = i1 expression || i2 expression

negative :: Inspection -> Inspection
negative f = not . f

scoped :: Inspection -> Identifier -> Inspection
scoped inspection scope =  any inspection . declarationsOf scope

scopedList :: Inspection -> [Identifier] -> Inspection
scopedList i =  foldl scoped i . reverse

transitive :: Inspection -> Identifier -> Inspection
transitive inspection identifier code = any (`scopedInspection` code) . transitiveReferencedIdentifiers identifier $ code
  where scopedInspection = scoped inspection

transitiveList :: Inspection -> [Identifier] -> Inspection
transitiveList i identifiers = transitive (scopedList i (init identifiers)) (last identifiers)

type GeneralizedInspection = Expression -> Inspection

generalize :: Inspection -> GeneralizedInspection
generalize inspection = \_ expression -> inspection expression

generalized :: (Inspection -> a -> Inspection) -> a -> GeneralizedInspection -> GeneralizedInspection
generalized f arg inspection = \root expression -> (f (inspection root) arg) expression

generalizedScoped :: Identifier -> GeneralizedInspection -> GeneralizedInspection
generalizedScoped = generalized scoped

generalizedScopedList :: [Identifier] -> GeneralizedInspection -> GeneralizedInspection
generalizedScopedList = generalized scopedList

generalizedTransitive :: Identifier -> GeneralizedInspection -> GeneralizedInspection
generalizedTransitive = generalized transitive

generalizedTransitiveList :: [Identifier] -> GeneralizedInspection -> GeneralizedInspection
generalizedTransitiveList = generalized transitiveList
