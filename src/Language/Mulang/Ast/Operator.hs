{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Ast.Operator (Operator(..)) where

import           GHC.Generics

data Operator
    = Equal
    -- equal operator
    | NotEqual
    -- ^ distinct operator
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


