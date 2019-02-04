module Language.Mulang.Inspector.Matcher (
  unmatching,
  thatEvery,
  that,
  Matcher) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Primitive (Inspection)

type Matcher = [Expression] -> Bool

thatEvery :: [Inspection] -> Matcher
thatEvery inspections expressions = and (zipWith ($) inspections expressions)

that :: Inspection -> Matcher
that = thatEvery . (:[])

unmatching :: (Matcher -> b) -> b
unmatching f = f (const True)
