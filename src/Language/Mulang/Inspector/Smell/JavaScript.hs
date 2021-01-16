module Language.Mulang.Inspector.Smell.JavaScript (
  usesVarInsteadOfLet,
  usesForInInsteadOfForOf) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector.Primitive (Inspection, containsExpression)


usesVarInsteadOfLet :: Inspection
usesVarInsteadOfLet = containsExpression f
  where f (Other (Just "JSVar") _)                              = True
        f (For [Generator (OtherPattern (Just "JSVar") _) _] _) = True
        f _                                                     = False

usesForInInsteadOfForOf :: Inspection
usesForInInsteadOfForOf = containsExpression f
  where f (Other (Just "JSForIn") _)                              = True
        f _                                                       = False
