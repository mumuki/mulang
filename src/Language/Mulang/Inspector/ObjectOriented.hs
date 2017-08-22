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
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Generic

implements :: IdentifierInspection
implements predicate = containsExpression f
  where f (Implement name) = predicate name
        f _                = False

includes :: IdentifierInspection
includes predicate = containsExpression f
  where f (Include name) = predicate name
        f _              = False

inherits :: IdentifierInspection
inherits predicate = containsExpression f
  where f (Class _ (Just name) _) = predicate name
        f _                       = False

instantiates :: IdentifierInspection
instantiates predicate = containsExpression f
  where f (New name _) = predicate name
        f _            = False

usesInheritance :: Inspection
usesInheritance = declaresSuperclass anyone

usesMixins :: Inspection
usesMixins = includes anyone

declaresObject :: IdentifierInspection
declaresObject =  containsDeclaration f
  where f (Object _ _) = True
        f _            = False

declaresSuperclass :: IdentifierInspection
declaresSuperclass = inherits

declaresClass :: IdentifierInspection
declaresClass =  containsDeclaration f
  where f (Class _ _ _) = True
        f _             = False

declaresEnumeration :: IdentifierInspection
declaresEnumeration =  containsDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

declaresInterface :: IdentifierInspection
declaresInterface =  containsDeclaration f
  where f (Interface _ _ _) = True
        f _                 = False

declaresAttribute :: IdentifierInspection
declaresAttribute =  containsDeclaration f
  where f (Attribute _ _) = True
        f _               = False

declaresMethod :: IdentifierInspection
declaresMethod =  containsDeclaration f
  where f (Method _ _) = True
        f _            = False
