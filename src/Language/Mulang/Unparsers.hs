module Language.Mulang.Unparsers(Unparser) where

import Language.Mulang.Ast (Expression)

type Unparser = Expression -> String
