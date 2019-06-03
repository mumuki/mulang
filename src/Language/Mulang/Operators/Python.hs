{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.Python (pythonTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast.Operator (Operator (..))

pythonTokensTable :: TokensTable
pythonTokensTable =
  buildTokensTable [
    (NotEqual, ["!=", "<>"]),
    (Negation, ["not"]),
    (And, ["and"]),
    (Or, ["or"]),
    (Hash, ["hash"])
  ]
