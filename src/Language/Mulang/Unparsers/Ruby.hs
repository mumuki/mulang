{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unparsers.Ruby (unrb) where
import Language.Mulang.Unparsers (Unparser)
import Language.Mulang.Ast

import Data.List (intercalate)

unrb :: Unparser
unrb = unparse

unparse :: Unparser
unparse MuNil                                                         = "nil"
unparse (Print exp)                                                   = "puts(" ++ unparse exp ++ ")"
unparse (Return exp)                                                  = "return " ++ unparse exp
unparse (MuNumber n)                                                  = show n
unparse MuTrue                                                        = "true"
unparse MuFalse                                                       = "false"
unparse (MuString s)                                                  = show s
unparse (MuList xs)                                                   = "[" ++ unparseMany xs ++ "]"
unparse (Assignment id value)                                         = id ++ " = " ++ unparse value
unparse (Reference id)                                                = id
unparse (Application (Primitive Negation) [bool])                     = "!" ++ unparse bool
unparse (Application (Reference "+") [arg1, arg2])                    = unparse arg1 ++ " + " ++ unparse arg2
unparse (Application (Reference "*") [arg1, arg2])                    = unparse arg1 ++ " * " ++ unparse arg2
unparse (Application (Reference "/") [arg1, arg2])                    = unparse arg1 ++ " / " ++ unparse arg2
unparse (Application (Reference "-") [arg1, arg2])                    = unparse arg1 ++ " - " ++ unparse arg2
unparse (Application (Reference id) args)                             = id ++ "(" ++ unparseMany args ++ ")"
unparse (Sequence xs)                                                 = intercalate "\n" . map unparse $ xs
unparse (While cond body)                                             = "while "++ unparse cond ++ "\n" ++ (tab . unparse) body ++ "end\n"
unparse (Raise None)                                                  = "raise"
unparse (Raise arg)                                                   = "raise " ++ unparse arg
unparse (Lambda params body)                                          = "lambda { |"++unparseParams params++"| "++ unparse body ++" }"
unparse (MuTuple args)                                                = "("++ unparseMany args++")"
unparse (Yield value)                                                 = "yield " ++ unparse value
unparse (Class id Nothing body)                                       = "class "++ id ++"\n" ++ (tab . unparse) body ++ "end\n"
unparse (Class id (Just parent) body)                                 = "class "++ id ++" < "++parent++"\n" ++ (tab . unparse) body ++ "end\n"
unparse None                                                          = ""
unparse (Send receptor (Reference id) args)                           = unparse receptor ++ "."++ id ++"("++ unparseMany args ++")"
unparse (If cond trueBody falseBody)                                  = "if "++ unparse cond ++"\n"++ (tab . unparse) trueBody ++ "else\n" ++ (tab . unparse) falseBody ++ "end\n"
unparse (SimpleFunction id args body)                                 = unparseDef id args body
unparse (SimpleProcedure id args body)                                = unparseDef id args body
unparse other                                                         = error . show $ other

unparseDef :: String -> [Pattern] -> Expression -> String
unparseDef id args body = "def "++ id ++"("++ unparseParams args++")\n" ++ (tab . unparse) body ++ "end\n"

unparseParams :: [Pattern] -> String
unparseParams = intercalate "," . map unparseParam

unparseParam :: Pattern -> String
unparseParam (VariablePattern id) = id
unparseParam other = error . show $ other

tab :: String -> String
tab = unlines . map ("\t"++) . lines

unparseMany :: [Expression] -> String
unparseMany = intercalate "," . map unparse
