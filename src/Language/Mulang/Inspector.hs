module Language.Mulang.Inspector (
  module Language.Mulang.Inspector.ObjectOriented,
  module Language.Mulang.Inspector.Generic,

-- functional
  usesComposition,
  usesGuards,
  usesLambda,

  usesPatternMatching,
  usesComprehension,


-- logic
  usesNot,
  usesFindall,
  usesForall,
  usesUnifyOperator,

  declaresFact,
  declaresRule,

-- imperative
  usesRepeat,
  usesWhile,
  usesSwitch,

  declaresProcedure) where


import Language.Mulang
import Language.Mulang.Binding
import Language.Mulang.Inspector.ObjectOriented
import Language.Mulang.Inspector.Generic


declaresFact :: BindingPredicate -> Inspection
declaresFact = containsDeclaration f
  where f (FactDeclaration _ _) = True
        f _                     = False

declaresRule :: BindingPredicate -> Inspection
declaresRule = containsDeclaration f
  where f (RuleDeclaration _ _ _) = True
        f _                       = False


declaresProcedure :: BindingPredicate -> Inspection
declaresProcedure = containsDeclaration f
  where f (ProcedureDeclaration _ _) = True
        f _                          = False


-- | Inspection that tells whether an expression uses the composition operator '.'
-- in its definition
usesComposition :: Inspection
usesComposition = containsExpression f
  where f (Variable ".") = True
        f _ = False

-- | Inspection that tells whether an expression uses guards
-- in its definition
usesGuards :: Inspection
usesGuards = containsBody f
  where f (GuardedBody _) = True
        f _ = False



-- | Inspection that tells whether an expression uses while
-- in its definition
usesWhile :: Inspection
usesWhile = containsExpression f
  where f (While _ _) = True
        f _ = False
-- | Inspection that tells whether an expression uses Switch
-- in its definition
usesSwitch :: Inspection
usesSwitch = containsExpression f
  where f (Switch _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses pattern matching
-- in its definition
usesPatternMatching :: Inspection
usesPatternMatching = containsExpression f
  where f (FunctionDeclaration _ equations) = any nonVariablePattern (patterns equations)
        f _ = False

        patterns = concatMap (\(Equation ps _) -> ps)

        nonVariablePattern :: Pattern -> Bool
        nonVariablePattern (VariablePattern _) = False
        nonVariablePattern _                   = True



-- | Inspection that tells whether an expression uses reoeat
-- in its definition
usesRepeat :: Inspection
usesRepeat = containsExpression f
  where f (Repeat _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses a lambda expression
-- in its definition
usesLambda :: Inspection
usesLambda = containsExpression f
  where f (Lambda _ _) = True
        f _ = False


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

usesUnifyOperator :: Inspection
usesUnifyOperator = containsExpression f
  where f (Exist "=" _) = True
        f _ = False




-- | Inspection that tells whether an expression uses
-- comprehensions - list comprehension, for comprehension, do-syntax, etc -
-- in its definitions
usesComprehension :: Inspection
usesComprehension = containsExpression f
  where f (Comprehension _ _) = True
        f _ = False