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
  declaresMethod,
  declaresPrimitive)  where

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator (Operator)
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Bound (BoundInspection, containsBoundDeclaration)
import Language.Mulang.Inspector.Primitive (Inspection, containsExpression, containsDeclaration)

implements :: BoundInspection
implements predicate = containsExpression f
  where f (Implement (Reference name)) = predicate name
        f _                            = False

includes :: BoundInspection
includes predicate = containsExpression f
  where f (Include (Reference name)) = predicate name
        f _                          = False

inherits :: BoundInspection
inherits predicate = containsDeclaration f
  where f (Class _ (Just name) _) = predicate name
        f _                       = False

instantiates :: BoundInspection
instantiates predicate = containsExpression f
  where f (New (Reference name) _) = predicate name
        f _                        = False

usesInheritance :: Inspection
usesInheritance = declaresSuperclass anyone

usesMixins :: Inspection
usesMixins = includes anyone

declaresObject :: BoundInspection
declaresObject =  containsBoundDeclaration f
  where f (Object _ _) = True
        f _            = False

declaresSuperclass :: BoundInspection
declaresSuperclass = inherits

declaresClass :: BoundInspection
declaresClass =  containsBoundDeclaration f
  where f (Class _ _ _) = True
        f _             = False

declaresEnumeration :: BoundInspection
declaresEnumeration =  containsBoundDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

declaresInterface :: BoundInspection
declaresInterface =  containsBoundDeclaration f
  where f (Interface _ _ _) = True
        f _                 = False

declaresAttribute :: BoundInspection
declaresAttribute =  containsBoundDeclaration f
  where f (Attribute _ _) = True
        f _               = False

declaresMethod :: BoundInspection
declaresMethod =  containsBoundDeclaration f
  where f (Method _ _) = True
        f _            = False

-- primitive can only be declared as methods
declaresPrimitive :: Operator -> Inspection
declaresPrimitive operator = containsExpression f
  where f (PrimitiveMethod o _) = operator == o
        f _                     = False
