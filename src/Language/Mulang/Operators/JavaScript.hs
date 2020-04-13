{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.JavaScript (javaScriptTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast.Operator (Operator (..))

javaScriptTokensTable :: TokensTable
javaScriptTokensTable =
  buildTokensTable [
    (Equal, ["==", "==="]),
    (NotEqual, ["!=", "!=="])
  ]
