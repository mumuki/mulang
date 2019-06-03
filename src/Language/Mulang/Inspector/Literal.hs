module Language.Mulang.Inspector.Literal (
  isNil,
  isNumber,
  isBool,
  isChar,
  isString,
  isSymbol) where

import Language.Mulang.Ast
import Language.Mulang.Inspector.Primitive (Inspection)

isNil :: Inspection
isNil = isLiteral MuNil

isNumber :: Double -> Inspection
isNumber = isLiteral . MuNumber

isBool :: Bool -> Inspection
isBool = isLiteral . MuBool

isString :: String -> Inspection
isString = isLiteral . MuString

isChar :: Char -> Inspection
isChar = isLiteral . MuChar

isSymbol :: String -> Inspection
isSymbol = isLiteral . MuSymbol

isLiteral :: Expression -> Inspection
isLiteral = (==)
