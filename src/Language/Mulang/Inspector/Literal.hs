module Language.Mulang.Inspector.Literal (
  literalMatcher,
  literalsMatcher,
  usesLiteral) where

import Language.Mulang.Ast
import Language.Mulang.Generator (expressions, declarations)
import Language.Mulang.Inspector.Primitive (containsExpression, Inspection, ValueMatcher (..), ArgumentsMatcher (..))

import Text.Read (readMaybe)

usesLiteral :: Code -> Inspection
usesLiteral = containsExpression . matchValue . literalMatcher

literalMatcher :: Code -> ValueMatcher
literalMatcher value = ValueMatcher f
  where f MuNil              = value == "Nil"
        f (MuNumber number)  = (readMaybe value) == Just number
        f (MuBool bool)      = (readMaybe value) == Just bool
        f (MuString string)  = (readMaybe value) == Just string
        f (MuChar char)      = (readMaybe value) == Just char
        f (MuSymbol string)  = (readMaybe ("\"" ++ value ++ "\"")) == Just ("#" ++ string)

literalsMatcher :: [Code] -> ArgumentsMatcher
literalsMatcher values = ArgumentsMatcher $ and . zipWith (matchValue . literalMatcher) values
