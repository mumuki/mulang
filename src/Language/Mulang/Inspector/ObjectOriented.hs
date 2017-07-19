module Language.Mulang.Inspector.ObjectOriented (
  usesInheritance,
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

usesInheritance :: Inspection
usesInheritance = declaresSuperclass anyone

declaresObject :: BindingPredicate -> Inspection
declaresObject =  containsDeclaration f
  where f (Object _ _) = True
        f _            = False

declaresSuperclass :: BindingPredicate -> Inspection
declaresSuperclass predicate = containsExpression f
  where f (Class _ (Just name) _) = predicate name
        f _                       = False

declaresClass :: BindingPredicate -> Inspection
declaresClass =  containsDeclaration f
  where f (Class _ _ _) = True
        f _             = False

declaresEnumeration :: BindingPredicate -> Inspection
declaresEnumeration =  containsDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

declaresInterface :: BindingPredicate -> Inspection
declaresInterface =  containsDeclaration f
  where f (Interface _ _ _) = True
        f _                 = False

declaresAttribute :: BindingPredicate -> Inspection
declaresAttribute =  containsDeclaration f
  where f (Attribute _ _) = True
        f _               = False

declaresMethod :: BindingPredicate -> Inspection
declaresMethod =  containsDeclaration f
  where f (Method _ _) = True
        f _            = False
