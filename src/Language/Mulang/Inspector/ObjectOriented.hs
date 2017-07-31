module Language.Mulang.Inspector.ObjectOriented (
  implements,
  includes,
  inherits,
  instantiates,
  usesInheritance,
  usesMixins,
  declaresObject,
  declaresSuperclass,
  declaresClass,
  declaresInterface,
  declaresEnumeration,
  declaresAttribute,
  declaresMethod)  where

import Language.Mulang.Ast
import Language.Mulang.Binding
import Language.Mulang.Inspector.Generic

implements :: BindedInspection
implements predicate = containsExpression f
  where f (Implement name) = predicate name
        f _                = False

includes :: BindedInspection
includes predicate = containsExpression f
  where f (Include name) = predicate name
        f _              = False

inherits :: BindedInspection
inherits predicate = containsExpression f
  where f (Class _ (Just name) _) = predicate name
        f _                       = False

instantiates :: BindedInspection
instantiates predicate = containsExpression f
  where f (New name _) = predicate name
        f _            = False

usesInheritance :: Inspection
usesInheritance = declaresSuperclass anyone

usesMixins :: Inspection
usesMixins = includes anyone

declaresObject :: BindedInspection
declaresObject =  containsDeclaration f
  where f (Object _ _) = True
        f _            = False

declaresSuperclass :: BindedInspection
declaresSuperclass = inherits

declaresClass :: BindedInspection
declaresClass =  containsDeclaration f
  where f (Class _ _ _) = True
        f _             = False

declaresEnumeration :: BindedInspection
declaresEnumeration =  containsDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

declaresInterface :: BindedInspection
declaresInterface =  containsDeclaration f
  where f (Interface _ _ _) = True
        f _                 = False

declaresAttribute :: BindedInspection
declaresAttribute =  containsDeclaration f
  where f (Attribute _ _) = True
        f _               = False

declaresMethod :: BindedInspection
declaresMethod =  containsDeclaration f
  where f (Method _ _) = True
        f _            = False
