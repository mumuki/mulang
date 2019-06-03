{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.Java (javaTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast.Operator (Operator (..))

javaTokensTable :: TokensTable
javaTokensTable = buildTokensTable [(Hash, ["hashCode"])]
