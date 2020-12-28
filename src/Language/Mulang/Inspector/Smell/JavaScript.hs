module Language.Mulang.Inspector.Smell.JavaScript (
  usesVarInsteadOfLet) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Primitive (Inspection, containsExpression)


usesVarInsteadOfLet :: Inspection
usesVarInsteadOfLet = containsExpression f
  where f (Other (Just "var") _)                              = True
        f (For [Generator (OtherPattern (Just "var") _) _] _) = True
        f _                                                   = False
