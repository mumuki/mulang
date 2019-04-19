{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unparsers.Java (unparseJava) where
import Language.Mulang.Unparsers (Unparser)
import Language.Mulang.Ast

import Data.List (intercalate)
import Data.String (unwords)

unparseJava :: Unparser
unparseJava = unparse

unparse :: Unparser
unparse (Class name Nothing body)           = unwords ["public class", name, "{", unbody body, "}"]
unparse (Class name (Just superclass) body) = unwords ["public class", name, "extends", superclass, "{", unbody body, "}"]
unparse (Interface name extends body)       = unwords ["public interface", name, unextends extends, "{", unbody body, "}"]
unparse _                                   = ""

unextends :: [Identifier] -> String
unextends [] = ""
unextends parents = unwords ["extends", intercalate "," parents]

unbody :: Unparser
unbody MuNil                                       = "null"
unbody None                                        = ""
unbody (SimpleMethod name [] None)                 = unwords ["public", "void", name, "()", "{}"]
unbody (Sequence members)                          = unlines (map unbody members)
unbody (SubroutineSignature name args typ [])      = unwords ["public abstract", typ, name, "(", unparam args, ");"]
unbody other                                       = error . show $ other

unparam :: [Identifier] -> String
unparam = intercalate "," . zipWith buildParam [0..]

buildParam :: Int -> Identifier -> String
buildParam index typeParam = typeParam ++ " arg" ++ show index
