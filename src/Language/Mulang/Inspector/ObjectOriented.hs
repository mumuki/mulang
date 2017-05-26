module Language.Mulang.Inspector.ObjectOriented (
  declaresObject,
  declaresAttribute,
  declaresMethod)  where

import Language.Mulang.Ast
import Language.Mulang.Binding
import Language.Mulang.Inspector.Generic

declaresObject :: BindingPredicate -> Inspection
declaresObject =  containsDeclaration f
  where f (ObjectDeclaration _ _) = True
        f _                       = False

declaresAttribute :: BindingPredicate -> Inspection
declaresAttribute =  containsDeclaration f
  where f (AttributeDeclaration _ _) = True
        f _                          = False

declaresMethod :: BindingPredicate -> Inspection
declaresMethod =  containsDeclaration f
  where f (MethodDeclaration _ _) = True
        f _                       = False
