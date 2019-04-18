{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unparsers.Ruby (unrb) where
import Language.Mulang.Unparsers (Unparser)
import Language.Mulang.Ast

import Data.List (intercalate)
import Data.String (unwords)

unrb :: Unparser
unrb (MuNumber n)                                                  = show n
unrb MuTrue                                                        = "true"
unrb MuFalse                                                       = "false"
unrb (MuString s)                                                  = show s
unrb (MuList xs)                                                   = "[" ++ unrbMany xs ++ "]"
unrb (Assignment id value)                                         = id ++ " = " ++ unrb value
unrb (Reference id)                                                = id
unrb (Application (Primitive Negation) [bool])                     = "!" ++ unrb bool
unrb (Application (Reference "+") [arg1, arg2])                    = unrb arg1 ++ " + " ++ unrb arg2
unrb (Application (Reference "*") [arg1, arg2])                    = unrb arg1 ++ " * " ++ unrb arg2
unrb (Application (Reference "/") [arg1, arg2])                    = unrb arg1 ++ " / " ++ unrb arg2
unrb (Application (Reference "-") [arg1, arg2])                    = unrb arg1 ++ " - " ++ unrb arg2
unrb (Application (Reference id) args)                             = id ++ "(" ++ unrbMany args ++ ")"
unrb (Sequence xs)                                                 = intercalate "\n" . map unrb $ xs
unrb (While cond body)                                             = "while "++ unrb cond ++ ":\n" ++ (tab . unrb) body
unrb  (For [Generator (TuplePattern [VariablePattern "x"]) generator] None) = "for x in "++ unrb generator ++": pass"
unrb (Raise None)                                                   = "raise"
unrb (Raise (Application (Reference id) [arg]))                     = "raise "++ id ++"("++ unrb arg++")"
unrb  (Lambda [VariablePattern "x"] body)                           = "lambda { |x| "++ unrb body ++" }"
unrb (MuTuple args)                                                 = "("++ unrbMany args++")"
unrb (Yield value)                                                  = "yield " ++ unrb value
unrb (Class id Nothing body)                                        = "class "++ id ++":\n" ++ (tab . unrb) body
unrb (Class id (Just parent) body)                                  = "class "++ id ++" < "++parent++"\n" ++ (tab . unrb) body ++ "\nend\n"
unrb None                                                           = "pass"
unrb (Send receptor (Reference id) args)                            =  unrb receptor ++ "."++ id ++"("++ unrbMany args ++")"
unrb (If cond trueBody falseBody)                                   = "if "++ unrb cond ++"\n"++ (tab . unrb) trueBody ++ "\nelse\n" ++ (tab . unrb) falseBody ++ "\nend\n"
unrb (SimpleFunction id args body)                                  = "def "++ id ++"("++ unrbParameters args++"):\n" ++ (tab . unrb) body ++ "\nend\n"

unrbParameters _ = ""

tab :: String -> String
tab = unlines . map ("\t"++) . lines

unrbMany :: [Expression] -> String
unrbMany = intercalate "," . map unrb
