module Language.Mulang.Inspector.Literal (isLiteral) where

import Language.Mulang.Ast
import Language.Mulang.Inspector.Primitive (Inspection)

import Text.Read (readMaybe)

isLiteral :: Code -> Inspection
isLiteral value = f
  where f MuNil              = value == "Nil"
        f (MuNumber number)  = (readMaybe value) == Just number
        f (MuBool bool)      = (readMaybe value) == Just bool
        f (MuString string)  = (readMaybe value) == Just string
        f (MuChar char)      = (readMaybe value) == Just char
        f (MuSymbol string)  = (readMaybe ("\"" ++ value ++ "\"")) == Just ("#" ++ string)
        f _                  = False
