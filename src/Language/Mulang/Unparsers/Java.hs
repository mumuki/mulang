{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Unparsers.Java (unjava) where
import Language.Mulang.Ast

import Data.List (intercalate)
import Data.String (unwords)

type Unparser = Expression -> String

unjava :: Unparser
unjava (Class name Nothing _ )           = unwords ["public class", name, "{}"]
unjava (Class name (Just superclass) _ ) = unwords ["public class", name, "extends", superclass, "{}"]
unjava (Interface name extends body)     = unwords ["public interface", name, unextends extends, "{", unbody body, "}"]
unjava _                                 = ""

unextends :: [Identifier] -> String
unextends [] = ""
unextends parents = unwords ["extends", intercalate "," parents]

unbody :: Unparser
unbody MuNull                        = ""
unbody (Sequence members)            = unlines (map unbody members)
unbody (TypeSignature name args typ) = unwords ["public abstract", typ, name, "(", unparam args, ");"]

unparam :: [Identifier] -> String
unparam = intercalate "," . zipWith buildParam [0..]

buildParam :: Int -> Identifier -> String
buildParam index typeParam = typeParam ++ " arg" ++ show index
