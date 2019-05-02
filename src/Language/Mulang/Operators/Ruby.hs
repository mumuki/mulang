{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.Ruby (rubyTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast (Operator (..))

rubyTokensTable :: TokensTable
rubyTokensTable =
  buildTokensTable [
    (And, ["&&", "and"]),
    (Or, ["||", "or"]),
    (Hash, ["hash"]),
    (ForwardComposition, [">>"]),
    (BackwardComposition, ["<<"])
  ]
