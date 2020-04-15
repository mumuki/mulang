{-# LANGUAGE DeriveGeneric, PatternSynonyms, ViewPatterns #-}

module Language.Mulang.Ast.Operator (
  Operator(..),
  pattern Equalish,
  pattern NotEqualish) where

import           GHC.Generics

data Operator
    = Equal
    -- ^ equal operator
    | NotEqual
    -- ^ distinct operator
    | Like
    -- ^ similar-ignoring-type operator
    | NotLike
    -- ^ not similar-ignoring-type operator
    | Same
    -- ^ reference-identical operator
    | NotSame
    -- ^ not reference-identical operator
    | Negation
    -- ^ not operator
    | And
    -- ^ and operator
    | Or
    -- ^ or operator
    | Hash
    -- ^ hashcode operator
    | GreatherOrEqualThan
    | GreatherThan
    | LessOrEqualThan
    | LessThan
    | Otherwise
    -- ^ guard's otherwise operator
    | Plus
    | Minus
    | Multiply
    | Divide
    | ForwardComposition
    -- (f >> g)(x) = (g . f)(x) = g(f(x)) operator
    | BackwardComposition
    -- (f << g)(x) = (f . g)(x) = f(g(x)) operator
    | Modulo
    -- ^ % operator
    | BitwiseOr
    -- ^ bit-level or operator |
    | BitwiseAnd
    -- ^ bit-level and operator &
    | BitwiseXor
    -- ^ bit-level xor operator
    | BitwiseLeftShift
    -- ^ bit-level left shift operator <<
    | BitwiseRightShift
    -- ^ bit-level right shift operator >>
  deriving (Eq, Show, Read, Generic, Ord, Enum)


pattern Equalish <- (isEqualish -> True)
pattern NotEqualish <- (isNotEqualish -> True)

isEqualish :: Operator -> Bool
isEqualish Equal = True
isEqualish Like  = True
isEqualish _     = False

isNotEqualish :: Operator -> Bool
isNotEqualish NotEqual = True
isNotEqualish NotLike = True
isNotEqualish _       = False
