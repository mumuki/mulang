module Language.Mulang.Inspector.Contextualized (
  decontextualize,
  contextualize,
  contextualized,
  contextualized2,
  contextualizedBind,
  boundContextualize,
  ContextualizedConsult,
  ContextualizedCounter,
  ContextualizedInspection,
  ContextualizedBoundConsult,
  ContextualizedBoundCounter,
  ContextualizedBoundInspection) where

import Data.Count (Count)
import Data.Function.Extra (compose2)

import Language.Mulang.Ast
import Language.Mulang.Consult (Consult)
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Bound (BoundConsult)

type ContextualizedConsult a = Expression -> Consult a
type ContextualizedCounter = ContextualizedConsult Count
type ContextualizedInspection = ContextualizedConsult Bool

type ContextualizedBoundConsult a = IdentifierPredicate -> ContextualizedConsult a
type ContextualizedBoundCounter = ContextualizedBoundConsult Count
type ContextualizedBoundInspection = ContextualizedBoundConsult Bool

--
-- Lifts
--

contextualize :: Consult a -> ContextualizedConsult a
contextualize = const

-- Generalized version of bind to accept contextualized inspections
contextualizedBind :: ContextualizedConsult a -> ContextualizedBoundConsult a
contextualizedBind = const

-- Generalized version of contextualize to accept bound inspections
boundContextualize :: BoundConsult a -> ContextualizedBoundConsult a
boundContextualize i = contextualize . i

--
-- Unlift
--

decontextualize :: ContextualizedConsult a -> Consult a
decontextualize inspection = \expression -> inspection expression expression

--
-- Modifiers
--

contextualized :: (Consult a -> Consult b) -> ContextualizedConsult a -> ContextualizedConsult b
contextualized = (.)

contextualized2 :: (Consult a -> Consult a -> Consult b) -> ContextualizedConsult a -> ContextualizedConsult a -> ContextualizedConsult b
contextualized2 = compose2
