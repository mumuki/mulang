module Language.Mulang.Counter (
  plus,
  counts,
  Count (..),
  Counter) where

import Data.Count (Count (..), counts)
import Data.Function.Extra (compose2)

import Language.Mulang.Consult (Consult)

type Counter = Consult Count

plus :: Counter -> Counter -> Counter
plus = compose2 (+)


