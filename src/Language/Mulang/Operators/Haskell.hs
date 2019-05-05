{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.Haskell (haskellTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast.Operator (Operator (..))

haskellTokensTable :: TokensTable
haskellTokensTable =
  buildTokensTable [
    (NotEqual, ["/="]),
    (Negation, ["not"]),
    (Otherwise, ["otherwise"]),
    (BackwardComposition, ["."]),
    (Divide, ["/", "div"])
  ]
