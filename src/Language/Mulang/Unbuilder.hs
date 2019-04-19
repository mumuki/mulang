{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unbuilder (
    tab,
    binary,
    parenthesize,
    number) where

tab :: String -> String
tab = unlines . map ("\t"++) . lines

binary :: String -> String -> String -> String
binary op arg1 arg2 = parenthesize . unwords $ [arg1, op, arg2]

parenthesize :: String -> String
parenthesize = ("("++) . (++")")

number :: Double -> String
number (properFraction -> (i, 0)) = show i
number n                          = show n
