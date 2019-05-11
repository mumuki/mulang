module Language.Mulang.Inspector.Contextualized (
  decontextualize,
  contextualize,
  contextualized,
  contextualized2,
  contextualizedBind,
  boundContextualize,
  ContextualizedInspection,
  ContextualizedModifier,
  ContextualizedBoundInspection) where

import Language.Mulang.Ast
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Bound (BoundInspection)
import Language.Mulang.Inspector.Combiner (Modifier, Modifier2)
import Language.Mulang.Inspector.Primitive (Inspection)

type ContextualizedInspection = Expression -> Inspection
type ContextualizedBoundInspection = IdentifierPredicate -> ContextualizedInspection
type ContextualizedModifier = ContextualizedInspection -> ContextualizedInspection
type ContextualizedModifier2 = ContextualizedInspection -> ContextualizedModifier

--
-- Lifts
--

contextualize :: Inspection -> ContextualizedInspection
contextualize = const

-- Generalized version of bind to accept contextualized inspections
contextualizedBind :: ContextualizedInspection -> ContextualizedBoundInspection
contextualizedBind = const

-- Generalized version of contextualize to accept bound inspections
boundContextualize :: BoundInspection -> ContextualizedBoundInspection
boundContextualize i = contextualize . i

--
-- Unlift
--

decontextualize :: ContextualizedInspection -> Inspection
decontextualize inspection = \expression -> inspection expression expression

--
-- Modifiers
--

contextualized :: Modifier -> ContextualizedModifier
contextualized f inspection = \context -> f (inspection context)

contextualized2 :: Modifier2 -> ContextualizedModifier2
contextualized2 f i1 i2 = \context -> f (i1 context) (i2 context)
