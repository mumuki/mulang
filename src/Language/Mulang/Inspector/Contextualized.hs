module Language.Mulang.Inspector.Contextualized (
  decontextualize,
  contextualize,
  contextualized,
  contextualizedBind,
  boundContextualized,
  contextualizedScoped,
  contextualizedScopedList,
  contextualizedTransitive,
  contextualizedTransitiveList,
  ContextualizedInspection,
  ContextualizedModifier,
  ContextualizedBoundInspection) where

import Language.Mulang.Ast
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Bound (BoundInspection)
import Language.Mulang.Inspector.Combiner (scoped, scopedList, transitive, transitiveList, Modifier)
import Language.Mulang.Inspector.Primitive (Inspection)

type ContextualizedInspection = Expression -> Inspection
type ContextualizedBoundInspection = IdentifierPredicate -> ContextualizedInspection
type ContextualizedModifier = ContextualizedInspection -> ContextualizedInspection

decontextualize :: ContextualizedInspection -> Inspection
decontextualize inspection = \expression -> inspection expression expression

contextualize :: Inspection -> ContextualizedInspection
contextualize = const

contextualized :: Modifier -> ContextualizedModifier
contextualized f inspection = \context -> f (inspection context)

contextualizedScoped :: Identifier -> ContextualizedModifier
contextualizedScoped scope = contextualized (scoped scope)

contextualizedScopedList :: [Identifier] -> ContextualizedModifier
contextualizedScopedList scope = contextualized (scopedList scope)

contextualizedTransitive :: Identifier -> ContextualizedModifier
contextualizedTransitive scope = contextualized (transitive scope)

contextualizedTransitiveList :: [Identifier] -> ContextualizedModifier
contextualizedTransitiveList scope = contextualized (transitiveList scope)

-- Generalized version of bind to accept contextualized inspections
contextualizedBind :: ContextualizedInspection -> ContextualizedBoundInspection
contextualizedBind = const

-- Generalized version of contextualize to accept bound inspections
boundContextualized :: BoundInspection -> ContextualizedBoundInspection
boundContextualized i = contextualize . i
