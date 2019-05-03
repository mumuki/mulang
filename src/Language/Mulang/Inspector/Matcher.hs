module Language.Mulang.Inspector.Matcher (
  unmatching,
  withEvery,
  with,
  Matcher) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Primitive (Inspection)

type Matcher = [Expression] -> Bool

-- | Creates a simple matcher that evaluates the given inspection only againts the first of its arguments
with :: Inspection -> Matcher
with inspection = inspection . head

-- | Creates a matcher using a list of inspections
-- The resultant matcher evaluates each inspection against each of its arguments
withEvery :: [Inspection] -> Matcher
withEvery inspections expressions = and (zipWith ($) inspections expressions)

unmatching :: (Matcher -> b) -> b
unmatching f = f (const True)

