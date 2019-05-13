module Language.Mulang.Inspector.Literal (
  isBool,
  isChar,
  isNil,
  isNumber,
  isSelf,
  isString,
  isSymbol,
  isLogic,
  isMath) where

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator (Operator (..))
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

isSelf :: Inspection
isSelf = isLiteral Self

isMath :: Inspection
isMath (Primitive Plus)     = True
isMath (Primitive Minus)    = True
isMath (Primitive Multiply) = True
isMath (Primitive Divide)   = True
isMath _                    = False

isLogic :: Inspection
isLogic (Primitive Negation) = True
isLogic (Primitive And)      = True
isLogic (Primitive Or)       = True
isLogic _                    = False

isLiteral :: Expression -> Inspection
isLiteral = (==)
