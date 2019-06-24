module Language.Mulang.Consult (Consult) where

import Language.Mulang.Ast (Expression)

type Consult a = Expression -> a
