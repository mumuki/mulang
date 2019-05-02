{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unbuilder (
    tab,
    binary,
    parenthesize,
    binaryOperator,
    number) where

import Language.Mulang.Operators (unparseOperator, Token, TokensTable)
import Language.Mulang.Ast (Operator)
import Data.Maybe (fromJust)

tab :: String -> String
tab = unlines . map ("\t"++) . lines

binary :: String -> String -> String -> String
binary op arg1 arg2 = parenthesize . unwords $ [arg1, op, arg2]

parenthesize :: String -> String
parenthesize = ("("++) . (++")")

number :: Double -> String
number (properFraction -> (i, 0)) = show i
number n                          = show n

binaryOperator :: Operator -> String -> String -> TokensTable -> String
binaryOperator op arg1 arg2 tokensTable = binary (fromJust $ unparseOperator op tokensTable) arg1 arg2
