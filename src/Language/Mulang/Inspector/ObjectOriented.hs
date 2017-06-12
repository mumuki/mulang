module Language.Mulang.Inspector.ObjectOriented (
  declaresObject,
  declaresClass,
  declaresAttribute,
  declaresMethod)  where

import Language.Mulang.Ast
import Language.Mulang.Binding
import Language.Mulang.Inspector.Generic

declaresObject :: BindingPredicate -> Inspection
declaresObject =  containsDeclaration f
  where f (Object _ _) = True
        f _            = False

declaresClass :: BindingPredicate -> Inspection
declaresClass =  containsDeclaration f
  where f (Class _ _) = True
        f _           = False

declaresAttribute :: BindingPredicate -> Inspection
declaresAttribute =  containsDeclaration f
  where f (Attribute _ _) = True
        f _               = False

declaresMethod :: BindingPredicate -> Inspection
declaresMethod =  containsDeclaration f
  where f (Method _ _) = True
        f _            = False
