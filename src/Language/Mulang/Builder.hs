module Language.Mulang.Builder (compact) where

import Language.Mulang

compact :: [Expression] -> Expression
compact [e] = e
compact es   = Sequence es