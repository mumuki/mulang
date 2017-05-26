module Language.Mulang.Inspector (
  module Language.Mulang.Inspector.Generic,
  module Language.Mulang.Inspector.ObjectOriented,
  module Language.Mulang.Inspector.Functional,
  module Language.Mulang.Inspector.Logic,

-- imperative
  usesRepeat,
  usesWhile,
  usesSwitch,

  declaresProcedure) where


import Language.Mulang.Ast
import Language.Mulang.Binding
import Language.Mulang.Inspector.Generic
import Language.Mulang.Inspector.ObjectOriented
import Language.Mulang.Inspector.Functional
import Language.Mulang.Inspector.Logic


declaresProcedure :: BindingPredicate -> Inspection
declaresProcedure = containsDeclaration f
  where f (ProcedureDeclaration _ _) = True
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
  where f (Switch _ _) = True
        f _ = False

-- | Inspection that tells whether an expression uses reoeat
-- in its definition
usesRepeat :: Inspection
usesRepeat = containsExpression f
  where f (Repeat _ _) = True
        f _ = False


