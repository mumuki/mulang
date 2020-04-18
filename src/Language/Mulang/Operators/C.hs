{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.C (cTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast.Operator (Operator (..))

cTokensTable :: TokensTable
cTokensTable =
  buildTokensTable [
    (BitwiseXor, ["^"])
  ]
