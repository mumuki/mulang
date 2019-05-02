{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unparsers.Java (unparseJava) where
import Language.Mulang.Unparsers (Unparser)
import Language.Mulang.Ast
import Language.Mulang.Unbuilder (tab, parenthesize, number, binary)

import Data.List (intercalate)
import Data.String (unwords)

unparseJava :: Unparser
unparseJava = unparse

unparseClassBody :: Unparser
unparseClassBody (Sequence xs) = unparseClassBodyMembers xs
unparseClassBody other = unparse other

unparseClassBodyMembers :: [Expression] -> String
unparseClassBodyMembers []     = ""
unparseClassBodyMembers (s@SubroutineSignature {}:m@SimpleMethod {}:xs) = unparseMethod s m ++ unparseClassBodyMembers xs
unparseClassBodyMembers (x:xs) = unparse x ++ unparseClassBodyMembers xs

unparseMethod :: Expression -> Expression -> String
unparseMethod (SubroutineSignature name args typ gens) (SimpleMethod name2 args2 body)
                                                    = unwords ["public", "void", name, "(", unparseParams args args2, ")", "{", unparse body, "}"]

unparseParams :: [Identifier] -> [Pattern] -> String
unparseParams types names = intercalate "," $ zipWith unparseParam types names

unparseParam :: Identifier -> Pattern -> [Char]
unparseParam typ (VariablePattern name) = unwords [typ, name]

unparseStatements :: Expression -> String
unparseStatements (Sequence xs) = unlines (map unparseStatements xs)
unparseStatements c@(Call {})   = unparse c ++ ";"
unparseStatements other         = unparse other

unparse :: Unparser
unparse (Class name Nothing body)                   = unwords ["public class", name, "{", (tab . unparseClassBody) body, "}"]
unparse (Class name (Just superclass) body)         = unwords ["public class", name, "extends", superclass, "{", (tab . unparseClassBody) body, "}"]
unparse (Interface name extends body)               = unwords ["public interface", name, unparseExtends extends, "{", (tab . unparse) body, "}"]
unparse MuNil                                       = "null"
unparse None                                        = ""
unparse Self                                        = "this"
unparse s@(Sequence members)                        = unparseStatements s
unparse (SubroutineSignature name args typ gens)    = unwords ["public abstract", unTypeParams gens, typ, name, "(", unparam args, ");"]
unparse (Print exp)                                 = "System.out.println(" ++ unparse exp ++ ");"
unparse (Return exp)                                = unwords ["return", unparse exp, ";"]
unparse (MuNumber n)                                = number n
unparse MuTrue                                      = "true"
unparse MuFalse                                     = "false"
unparse (MuString s)                                = show s
unparse (Reference id)                              = id
unparse (Assignment id value)                       = unwords [id, "=", unparse value, ";"]
unparse (Call (Reference "+") [arg1, arg2])         = binary "+" (unparse arg1) (unparse arg2)
unparse (Call (Reference "*") [arg1, arg2])         = binary "*" (unparse arg1) (unparse arg2)
unparse (Call (Reference "/") [arg1, arg2])         = binary "/" (unparse arg1) (unparse arg2)
unparse (Call (Reference "-") [arg1, arg2])         = binary "-" (unparse arg1) (unparse arg2)
unparse (Call (Primitive And) [arg1, arg2])         = binary "&&" (unparse arg1) (unparse arg2)
unparse (Call (Primitive Or) [arg1, arg2])          = binary "||" (unparse arg1) (unparse arg2)
unparse (Call (Primitive Negation) [arg1])          = parenthesize $ "!" ++ unparse arg1
unparse (Send receptor (Reference id) args)         = unparse receptor ++ "."++ id ++"("++ unparseMany args ++")"
unparse (While cond body)                           = unwords ["while", "(", unparse cond, ")", "{", (tab . unparseStatements) body, "}"]
unparse (Raise arg)                                 = unwords ["throw", unparse arg, ";"]
-- unparse (Lambda params body)                                          = "lambda { |"++unparseParams params++"| "++ unparse body ++" }"
-- unparse (If cond trueBody falseBody)                                  = "if "++ unparse cond ++"\n"++ (tab . unparse) trueBody ++ "else\n" ++ (tab . unparse) falseBody ++ "end\n"
unparse other                                       = error . show $ other

unparseMany :: [Expression] -> String
unparseMany = intercalate "," . map unparse

unparseExtends :: [Identifier] -> String
unparseExtends [] = ""
unparseExtends parents = unwords ["extends", intercalate "," parents]

unTypeParams :: [String] -> String
unTypeParams = unwords . map (("<"++).(++">"))

unparam :: [Identifier] -> String
unparam = intercalate "," . zipWith buildParam [0..]
  where
    buildParam :: Int -> Identifier -> String
    buildParam index typeParam = typeParam ++ " arg" ++ show index
