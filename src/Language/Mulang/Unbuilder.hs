module Language.Mulang.Unbuilder (
    tab,
    binary,
    parenthesize) where

tab :: String -> String
tab = unlines . map ("\t"++) . lines

binary :: String -> String -> String -> String
binary op arg1 arg2 = parenthesize . unwords $ [arg1, op, arg2]

parenthesize :: String -> String
parenthesize = ("("++) . (++")")
