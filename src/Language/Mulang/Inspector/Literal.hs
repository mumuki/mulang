module Language.Mulang.Inspector.Literal (
  isBool,
  isChar,
  isNil,
  isNumber,
  isSelf,
  isString,
  isSymbol,
  isLogic,
  isMath,
  isLiteral,
  isNonliteral) where

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator (Operator (..))
import Language.Mulang.Inspector.Primitive (Inspection)

isNil :: Inspection
isNil = (==) MuNil

isNumber :: Double -> Inspection
isNumber = (==) . MuNumber

isBool :: Bool -> Inspection
isBool = (==) . MuBool

isString :: String -> Inspection
isString = (==) . MuString

isChar :: Char -> Inspection
isChar = (==) . MuChar

isSymbol :: String -> Inspection
isSymbol = (==) . MuSymbol

isSelf :: Inspection
isSelf = (==) Self

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

isLiteral :: Inspection
isLiteral (MuBool _)   = True
isLiteral (MuChar _)   = True
isLiteral (MuNumber _) = True
isLiteral (MuString _) = True
isLiteral (MuSymbol _) = True
isLiteral MuNil        = True
isLiteral Self         = True
isLiteral _            = False

isNonliteral :: Inspection
isNonliteral = not . isLiteral
