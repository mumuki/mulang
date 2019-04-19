{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.Java (javaTokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast (PrimitiveOperator (..))

javaTokensTable :: TokensTable
javaTokensTable = buildTokensTable [(Hash, ["hashCode"])]
