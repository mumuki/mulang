{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unparsers.Python (unpy) where
import Language.Mulang.Unparsers (Unparser)
import Language.Mulang.Ast

import Data.List (intercalate)
import Data.String (unwords)

unpy :: Unparser
unpy (MuNumber n)                                                  = show n
unpy (MuBool b)                                                    = show b
unpy (MuString s)                                                  = show s
unpy (MuList xs)                                                   = "[" ++ unpyMany xs ++ "]"
unpy (Assignment id value)                                         = id ++ " = " ++ unpy value
unpy (Reference id)                                                = id
unpy (Application (Primitive Negation) [bool])                     = "not " ++ unpy bool
unpy (Application (Reference "+") [arg1, arg2])                    = unpy arg1 ++ " + " ++ unpy arg2
unpy (Application (Reference "*") [arg1, arg2])                    = unpy arg1 ++ " * " ++ unpy arg2
unpy (Application (Reference "/") [arg1, arg2])                    = unpy arg1 ++ " / " ++ unpy arg2
unpy (Application (Reference "-") [arg1, arg2])                    = unpy arg1 ++ " - " ++ unpy arg2
unpy (Application (Reference id) args)                             = id ++ "(" ++ unpyMany args ++ ")"
unpy (Sequence xs)                                                 = intercalate "\n" . map unpy $ xs
unpy (While cond body)                                             = "while "++ unpy cond ++ ":\n" ++ (tab . unpy) body
unpy  (For [Generator (TuplePattern [VariablePattern "x"]) generator] None) = "for x in "++ unpy generator ++": pass"
unpy (Raise None)                                                   = "raise"
unpy (Raise (Application (Reference id) [arg]))                     = "raise "++ id ++"("++ unpy arg++")"
unpy  (Lambda [VariablePattern "x"] (MuNumber 1))                   = "lambda x: 1"
unpy (MuTuple args)                                                 = "("++ unpyMany args++")"
unpy (Yield value)                                                  = "yield " ++ unpy value
unpy (Class id Nothing body)                                        = "class "++ id ++":\n" ++ (tab . unpy) body
unpy (Class id (Just parent) body)                                  = "class "++ id ++"("++parent++"):\n" ++ (tab . unpy) body
unpy None                                                           = "pass"
unpy (Send receptor (Reference id) args)                            =  unpy receptor ++ "."++ id ++"("++ unpyMany args ++")"
unpy (If cond trueBody falseBody)                                   = "if "++ unpy cond ++":\n"++ (tab . unpy) trueBody ++ "\nelse:\n" ++ (tab . unpy) falseBody
unpy (SimpleFunction id args body)                                  = "def "++ id ++"("++ unpyParameters args++"):\n" ++ (tab . unpy) body

unpyParameters _ = ""

tab :: String -> String
tab = unlines . map ("\t"++) . lines

unpyMany :: [Expression] -> String
unpyMany = intercalate "," . map unpy
