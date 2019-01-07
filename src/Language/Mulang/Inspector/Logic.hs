module Language.Mulang.Inspector.Logic (
  usesNot,
  usesFindall,
  usesForall,
  usesUnificationOperator,
  hasRedundantReduction,
  usesCut,
  usesFail,
  declaresFact,
  declaresRule,
  declaresPredicate) where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive (Inspection, containsExpression)
import Language.Mulang.Inspector.Bound (BoundInspection, containsBoundDeclaration)
import Language.Mulang.Inspector.Generic (uses)
import Language.Mulang.Inspector.Combiner (alternative)

declaresFact :: BoundInspection
declaresFact = containsBoundDeclaration f
  where f (Fact _ _) = True
        f _          = False

declaresRule :: BoundInspection
declaresRule = containsBoundDeclaration f
  where f (Rule _ _ _) = True
        f _            = False

usesNot :: Inspection
usesNot = containsExpression f
  where f (Not  _) = True
        f _ = False

usesFindall :: Inspection
usesFindall = containsExpression f
  where f (Findall  _ _ _) = True
        f _ = False

usesForall :: Inspection
usesForall = containsExpression f
  where f (Forall  _ _) = True
        f _ = False

usesUnificationOperator :: Inspection
usesUnificationOperator = uses (named "=")

usesCut :: Inspection
usesCut = uses (named "!")

usesFail :: Inspection
usesFail = uses (named "fail")

hasRedundantReduction :: Inspection
hasRedundantReduction = containsExpression f
  where f (Exist "is" [(VariablePattern _), (ApplicationPattern _ _)]) = False
        f (Exist "is" [(VariablePattern _), _]) = True
        f _ = False

declaresPredicate :: BoundInspection
declaresPredicate pred = alternative (declaresFact pred) (declaresRule pred)
