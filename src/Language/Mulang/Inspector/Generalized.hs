module Language.Mulang.Inspector.Generalized (
  generalize,
  generalized,
  generalizedScoped,
  generalizedScopedList,
  generalizedTransitive,
  generalizedTransitiveList,
  GeneralizedInspection) where

import Language.Mulang.Ast (Expression, Identifier)
import Language.Mulang.Inspector.Primitive (Inspection)
import Language.Mulang.Inspector.Combiner (scoped, scopedList, transitive, transitiveList, Scope)

type GeneralizedInspection = Expression -> Inspection

generalize :: Inspection -> GeneralizedInspection
generalize inspection = \_ expression -> inspection expression

generalized :: Scope -> GeneralizedInspection -> GeneralizedInspection
generalized f inspection = \root expression -> (f (inspection root)) expression

generalizedScoped :: Identifier -> GeneralizedInspection -> GeneralizedInspection
generalizedScoped scope = generalized (scoped scope)

generalizedScopedList :: [Identifier] -> GeneralizedInspection -> GeneralizedInspection
generalizedScopedList scope = generalized (scopedList scope)

generalizedTransitive :: Identifier -> GeneralizedInspection -> GeneralizedInspection
generalizedTransitive scope = generalized (transitive scope)

generalizedTransitiveList :: [Identifier] -> GeneralizedInspection -> GeneralizedInspection
generalizedTransitiveList scope = generalized (transitiveList scope)
