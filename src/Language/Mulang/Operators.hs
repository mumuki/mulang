{-# LANGUAGE TupleSections #-}

module Language.Mulang.Operators (
  buildTokensTable,
  buildOperatorsTable,
  defaultTokensTable,
  parseOperator,
  unparseOperator,
  Token,
  TokensTable,
  OperatorsTable) where

import           Language.Mulang.Ast.Operator (Operator (..))

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Tuple (swap)


type Token = String

type TokensTable = Map Operator [Token]
type OperatorsTable = Map Token Operator

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
    (LessThan, ["<"]),
    (Plus, ["+"]),
    (Minus, ["-"]),
    (Multiply, ["*"]),
    (Divide, ["/"]),
    (Mod, ["%"]),
    (BitwiseOr, ["|"]),
    (BitwiseAnd, ["&"]),
    (BitwiseLeftShift, ["<<"]),
    (BitwiseRightShift, [">>"])
  ]

buildTokensTable :: [(Operator, [Token])] -> TokensTable
buildTokensTable = flip Map.union defaultTokensTable  . Map.fromList

buildOperatorsTable :: TokensTable -> OperatorsTable
buildOperatorsTable =  Map.fromList . concatMap (fill . swap) . Map.toList
  where
    fill (xs, t) = map (,t) xs

unparseOperator :: Operator -> TokensTable -> Maybe Token
unparseOperator target = fmap head . (Map.lookup target)

parseOperator :: Token -> TokensTable -> Maybe Operator
parseOperator target =  (Map.lookup target) . buildOperatorsTable



