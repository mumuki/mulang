module Language.Mulang.Inspector.Matcher (
  unmatching,
  thatEvery,
  that,
  with,
  withEvery,
  Matcher) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Primitive (Inspection)
import           Language.Mulang.Inspector.Literal (isLiteral)

type Matcher = [Expression] -> Bool

thatEvery :: [Inspection] -> Matcher
thatEvery inspections expressions = and (zipWith ($) inspections expressions)

that :: Inspection -> Matcher
that inspection = inspection . head

unmatching :: (Matcher -> b) -> b
unmatching f = f (const True)

-- Special, simplified matchers

with :: Code -> Matcher
with = that . isLiteral

withEvery :: [Code] -> Matcher
withEvery = thatEvery . map isLiteral
