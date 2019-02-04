module Language.Mulang.Inspector.Literal (
  isLiteral,
  areLiterals,
  usesLiteral) where

import Language.Mulang.Ast
import Language.Mulang.Generator (expressions, declarations)
import Language.Mulang.Inspector.Primitive (containsExpression, Inspection)
import Language.Mulang.Inspector.Multi (allMatch, MultiInspection)

import Text.Read (readMaybe)

usesLiteral :: Code -> Inspection
usesLiteral value = containsExpression (isLiteral value)

isLiteral :: Code -> Inspection
isLiteral value = f
  where f MuNil              = value == "Nil"
        f (MuNumber number)  = (readMaybe value) == Just number
        f (MuBool bool)      = (readMaybe value) == Just bool
        f (MuString string)  = (readMaybe value) == Just string
        f (MuChar char)      = (readMaybe value) == Just char
        f (MuSymbol string)  = (readMaybe ("\"" ++ value ++ "\"")) == Just ("#" ++ string)

areLiterals :: [Code] -> MultiInspection
areLiterals codes = allMatch (map isLiteral codes)
