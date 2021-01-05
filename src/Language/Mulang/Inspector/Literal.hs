module Language.Mulang.Inspector.Literal (
  isAnything,
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
  isSimple,
  isCompound,
  isNonliteral,
  isOther) where

import Data.Function.Extra (orElse)

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator (Operator (..))
import Language.Mulang.Inspector.Primitive (Inspection)

isAnything :: Inspection
isAnything = const True

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
isLiteral = isSimple `orElse` isCompound

isSimple :: Inspection
isSimple (MuBool _)   = True
isSimple (MuChar _)   = True
isSimple (MuNumber _) = True
isSimple (MuString _) = True
isSimple (MuSymbol _) = True
isSimple MuNil        = True
isSimple Self         = True
isSimple _            = False

isCompound :: Inspection
isCompound (MuDict   _) = True
isCompound (MuList   _) = True
isCompound (MuObject _) = True
isCompound (MuTuple  _) = True
isCompound _            = False

isNonliteral :: Inspection
isNonliteral = not . isLiteral

isOther :: Inspection
isOther (Other _ _) = True
isOther _           = False
