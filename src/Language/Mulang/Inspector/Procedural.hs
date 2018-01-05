module Language.Mulang.Inspector.Procedural (
  usesRepeat,
  usesWhile,
  usesSwitch,
  usesForEach,
  declaresProcedure) where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Generic

declaresProcedure :: IdentifierPredicate -> Inspection
declaresProcedure = containsBoundDeclaration f
  where f (Procedure _ _) = True
        f _                          = False


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
  where f (Switch _ _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses reoeat
-- in its definition
usesRepeat :: Inspection
usesRepeat = containsExpression f
  where f (Repeat _ _) = True
        f _ = False

usesForEach :: Inspection
usesForEach = containsExpression f
  where f (For _ e) = not $ usesYield e
        f _         = False
