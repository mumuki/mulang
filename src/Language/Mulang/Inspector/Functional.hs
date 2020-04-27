module Language.Mulang.Inspector.Functional (
  countLambdas,
  usesComposition,
  usesComprehension,
  usesConditional,
  usesForComprehension,
  usesGuards,
  usesLambda,
  usesLambdaMatching,
  usesPatternMatching) where

import Data.Function.Extra (orElse)

import Language.Mulang.Ast hiding (Equal, NotEqual)
import Language.Mulang.Ast.Operator (Operator (..))
import Language.Mulang.Inspector.Primitive (Inspection, containsExpression, containsBody)
import Language.Mulang.Inspector.Generic (usesIf, usesYield)
import Language.Mulang.Inspector.Combiner (deriveUses, InspectionFamily)

usesConditional :: Inspection
usesConditional = orElse usesIf usesGuards


-- | Inspection that tells whether an expression uses the composition operator '.'
-- in its definition
usesComposition :: Inspection
usesComposition = containsExpression f
  where f (Primitive BackwardComposition) = True
        f (Primitive ForwardComposition)  = True
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
usesForComprehension :: Inspection
usesForComprehension = containsExpression f
  where f (For _ e) = usesYield e
        f _         = False

-- alias for usesForComprehension inspection
usesComprehension :: Inspection
usesComprehension = usesForComprehension

-- | Inspection that tells whether an expression uses a lambda expression
-- in its definition
(usesLambda, usesLambdaMatching, countLambdas) = deriveUses f :: InspectionFamily
  where f matcher (Lambda _ e) = matcher [e]
        f _       _            = False

-- | Inspection that tells whether an expression uses guards
-- in its definition
usesGuards :: Inspection
usesGuards = containsBody f
  where f (GuardedBody _) = True
        f _ = False
