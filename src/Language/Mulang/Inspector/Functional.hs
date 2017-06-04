module Language.Mulang.Inspector.Functional (
  usesGuards,
  usesComposition,
  usesLambda,
  usesPatternMatching,
  usesComprehension,
  usesConditional) where

import Language.Mulang.Ast
import Language.Mulang.Inspector.Generic
import Language.Mulang.Inspector.Combiner

usesConditional :: Inspection
usesConditional = alternative usesIf usesGuards


-- | Inspection that tells whether an expression uses the composition operator '.'
-- in its definition
usesComposition :: Inspection
usesComposition = containsExpression f
  where f (Reference ".") = True
        f _ = False

-- | Inspection that tells whether an expression uses pattern matching
-- in its definition
usesPatternMatching :: Inspection
usesPatternMatching = containsExpression f
  where f (Function _ equations) = any nonVariablePattern (patterns equations)
        f _ = False

        patterns = concatMap (\(Equation ps _) -> ps)

        nonVariablePattern :: Pattern -> Bool
        nonVariablePattern (VariablePattern _) = False
        nonVariablePattern _                   = True


-- | Inspection that tells whether an expression uses
-- comprehensions - list comprehension, for comprehension, do-syntax, etc -
-- in its definitions
usesComprehension :: Inspection
usesComprehension = containsExpression f
  where f (Comprehension _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses a lambda expression
-- in its definition
usesLambda :: Inspection
usesLambda = containsExpression f
  where f (Lambda _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses guards
-- in its definition
usesGuards :: Inspection
usesGuards = containsBody f
  where f (GuardedBody _) = True
        f _ = False
