module Language.Mulang.Builder (
    merge,
    compact,
    compactMap,
    compactConcatMap) where

import Language.Mulang.Ast

compactConcatMap :: (a -> [Expression]) -> [a] -> Expression
compactConcatMap f = compact . concat . map f

compactMap :: (a -> Expression) -> [a] -> Expression
compactMap f = compact . map f

compact :: [Expression] -> Expression
compact []  = None
compact [e] = e
compact es  = Sequence es

merge :: Expression -> Expression -> Expression
merge e1 None                      = e1
merge None e2                      = e2
merge (Sequence s1) (Sequence s2)  = Sequence (s1 ++ s2)
merge (Sequence s1) e2             = Sequence (s1 ++ [e2])
merge e1            (Sequence s2)  = Sequence (e1 : s2)
merge e1            e2             = Sequence [e1, e2]
