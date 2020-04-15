{-# LANGUAGE DeriveGeneric, PatternSynonyms, ViewPatterns #-}

module Language.Mulang.Ast.Operator (
  Operator(..),
  pattern Equalish,
  pattern NotEqualish) where

import           GHC.Generics

data Operator
    = Equal
    -- ^ `===`-like equal operator
    | NotEqual
    -- ^ `!==`-like distinct operator
    | Like
    -- ^ equal-ignoring-type operator
    | NotLike
    -- ^ not equal-ignoring-type operator
    | Same
    -- ^ reference-identical operator
    | NotSame
    -- ^ not reference-identical operator
    | Negation
    -- ^ `!`-like not operator
    | And
    -- ^ `&&`-like and operator
    | Or
    -- ^ `||`-like or operator
    | Hash
    -- ^ hashcode operator
    | GreatherOrEqualThan
    -- ^ `>=` operator
    | GreatherThan
    -- ^ `>` operator
    | LessOrEqualThan
    -- ^ `<=` operator
    | LessThan
    -- ^ `<` operator
    | Otherwise
    -- ^ guard's otherwise operator
    | Plus
    -- ^ numeric `+` operator
    | Minus
    -- ^ numeric `-` operator
    | Multiply
    -- ^ numeric `*` operator
    | Divide
    -- ^ numeric `/` operator
    | ForwardComposition
    -- (f >> g)(x) = (g . f)(x) = g(f(x)) operator
    | BackwardComposition
    -- (f << g)(x) = (f . g)(x) = f(g(x)) operator
    | Modulo
    -- ^ numeric `%-like` modulo operator
    | BitwiseOr
    -- ^ bit-level `|`-like or operator
    | BitwiseAnd
    -- ^ bit-level `&`-like and operator
    | BitwiseXor
    -- ^ bit-level `^`-like xor operator
    | BitwiseLeftShift
    -- ^ bit-level left `<<`-like shift operator
    | BitwiseRightShift
    -- ^ bit-level right `>>`-like shift operator
    | Absolute
    -- ^ numeric `abs`-like absolute operator
    | Round
    -- ^ numeric `round`-like round operator
    | Ceil
    -- ^ numeric `ceil`-like ceiling operator
    | Floor
    -- ^ numeric `ceil`-like floor operator
    | Max
    -- ^ `max`-like maximum value binary operator
    | Min
    -- ^ `min`-like minimum value binary operator
    | Size
    -- ^ collection `length`-like size operator
    | Detect
    -- ^ collection `find`-like search operator
    | DetectMax
    -- ^ collection `max`-like maximum operator
    | DetectMin
    -- ^ collection `min`-like minumum operator
    | Count
    -- ^ collection `count`-like operator
    | Select
    -- ^ collection `filter`-like operator
    | Collect
    -- ^ collection `map`-like operator
    | Inject
    -- ^ collection `reduce`-like / `fold`-like operator
    | AllSatisfy
    -- ^ collection `all`-like / `every`-like operator
    | AnySatisfy
    -- ^ collection `any`-like / `some`-like operator
    | Flatten
    -- ^ collection `flatten`-like operator
    | Gather
    -- ^ collection `flatmap`-like operator
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
