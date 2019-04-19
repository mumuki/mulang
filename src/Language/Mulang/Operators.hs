{-# LANGUAGE TupleSections #-}

module Language.Mulang.Operators (
  buildTokensTable,
  buildOperatorsTable,
  Token,
  TokensTable,
  OperatorsTable) where

import           Language.Mulang.Ast (PrimitiveOperator (..))

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tuple (swap)


type Token = String

type TokensTable = Map PrimitiveOperator [Token]
type OperatorsTable = Map Token PrimitiveOperator

-- C-style tokens
defaultTokensTable :: TokensTable
defaultTokensTable =
  Map.fromList [
    (Equal, ["=="]),
    (NotEqual, ["!="]),
    (Negation, ["!"]),
    (And, ["&&"]),
    (Or, ["||"]),
    (GreatherOrEqualThan, [">="]),
    (GreatherThan, [">"]),
    (LessOrEqualThan, ["<="]),
    (LessThan, ["<"])
  ]

buildTokensTable :: [(PrimitiveOperator, [Token])] -> TokensTable
buildTokensTable = flip Map.union defaultTokensTable  . Map.fromList

buildOperatorsTable :: TokensTable -> OperatorsTable
buildOperatorsTable =  Map.fromList . concatMap (fill . swap) . Map.toList
  where
    fill (xs, t) = map (,t) xs



