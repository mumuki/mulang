module Language.Mulang.Inspector.Contextualized (
  decontextualize,
  contextualize,
  contextualized,
  contextualized2,
  contextualizedBind,
  boundContextualize,
  ContextualizedInspection,
  ContextualizedBoundInspection) where

import Data.Function.Extra (compose2)
import Language.Mulang.Ast
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Bound (BoundInspection)
import Language.Mulang.Inspector.Primitive (Inspection)

type ContextualizedInspection = Expression -> Inspection
type ContextualizedBoundInspection = IdentifierPredicate -> ContextualizedInspection

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

contextualized :: (Inspection -> Inspection) -> ContextualizedInspection -> ContextualizedInspection
contextualized = (.)

contextualized2 :: (Inspection -> Inspection -> Inspection) -> ContextualizedInspection -> ContextualizedInspection -> ContextualizedInspection
contextualized2 = compose2
