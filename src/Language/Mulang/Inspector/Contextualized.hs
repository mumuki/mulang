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
  ContextualizedScope,
  ContextualizedBoundInspection) where

import Language.Mulang.Ast
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Bound (BoundInspection)
import Language.Mulang.Inspector.Combiner (scoped, scopedList, transitive, transitiveList, Scope)
import Language.Mulang.Inspector.Primitive (Inspection)

type ContextualizedInspection = Expression -> Inspection
type ContextualizedBoundInspection = IdentifierPredicate -> ContextualizedInspection
type ContextualizedScope = ContextualizedInspection -> ContextualizedInspection

decontextualize :: ContextualizedInspection -> Inspection
decontextualize inspection = \expression -> inspection expression expression

contextualize :: Inspection -> ContextualizedInspection
contextualize = const

contextualized :: Scope -> ContextualizedScope
contextualized f inspection = \context -> f (inspection context)

contextualizedScoped :: Identifier -> ContextualizedScope
contextualizedScoped scope = contextualized (scoped scope)

contextualizedScopedList :: [Identifier] -> ContextualizedScope
contextualizedScopedList scope = contextualized (scopedList scope)

contextualizedTransitive :: Identifier -> ContextualizedScope
contextualizedTransitive scope = contextualized (transitive scope)

contextualizedTransitiveList :: [Identifier] -> ContextualizedScope
contextualizedTransitiveList scope = contextualized (transitiveList scope)

-- Generalized version of bind to accept contextualized inspections
contextualizedBind :: ContextualizedInspection -> ContextualizedBoundInspection
contextualizedBind = const

-- Generalized version of contextualize to accept bound inspections
boundContextualized :: BoundInspection -> ContextualizedBoundInspection
boundContextualized i = contextualize . i
