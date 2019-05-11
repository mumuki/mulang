module Language.Mulang.Counter (
  plus,
  atLeast,
  atMost,
  exactly,
  Count (..),
  Counter) where

import Data.Count (Count (..))
import Data.Function.Extra (compose2)

import Language.Mulang.Ast (Expression)
import Language.Mulang.Identifier (IdentifierPredicate)
import Language.Mulang.Inspector.Contextualized (ContextualizedInspection)

type Counter = Expression -> Expression -> Count
type BoundCounter = IdentifierPredicate -> Counter

plus :: Counter -> Counter -> Counter
plus = compose2 (compose2 (+))

atLeast :: Count -> Counter -> ContextualizedInspection
atLeast n = limit (>= n)

atMost :: Count -> Counter -> ContextualizedInspection
atMost n = limit (<= n)

exactly :: Count -> Counter -> ContextualizedInspection
exactly n = limit (== n)

limit f counter context = f . counter context
