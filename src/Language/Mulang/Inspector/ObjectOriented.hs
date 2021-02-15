module Language.Mulang.Inspector.ObjectOriented (
  countAttributes,
  countClasses,
  countInterfaces,
  countMethods,
  countObjects,
  implements,
  includes,
  inherits,
  instantiates,
  usesInheritance,
  usesMixins,
  declaresAttribute,
  declaresAttributeMatching,
  declaresClass,
  declaresClassMatching,
  declaresEnumeration,
  declaresInterface,
  declaresInterfaceMatching,
  declaresMethod,
  declaresMethodMatching,
  declaresObject,
  declaresObjectMatching,
  declaresSuperclass,
  declaresPrimitive)  where

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator (Operator)
import Language.Mulang.Generator (equationsExpandedExpressions)
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Matcher (matches)
import Language.Mulang.Inspector.Bound (BoundInspection, containsBoundDeclaration)
import Language.Mulang.Inspector.Primitive (Inspection, containsExpression, containsDeclaration)
import Language.Mulang.Inspector.Family (deriveDeclares, BoundInspectionFamily)

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

(declaresObject, declaresObjectMatching, countObjects) = deriveDeclares f :: BoundInspectionFamily
  where f matcher (Object _ body) = matcher [body]
        f _        _              = False

declaresSuperclass :: BoundInspection
declaresSuperclass = inherits

(declaresClass, declaresClassMatching, countClasses) = deriveDeclares f :: BoundInspectionFamily
  where f matcher (Class _ _ body) = matcher [body]
        f _        _               = False

declaresEnumeration :: BoundInspection
declaresEnumeration =  containsBoundDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

(declaresInterface, declaresInterfaceMatching, countInterfaces) = deriveDeclares f :: BoundInspectionFamily
  where f matcher (Interface _ _ body) = matcher [body]
        f _        _                    = False

(declaresAttribute, declaresAttributeMatching, countAttributes) = deriveDeclares f :: BoundInspectionFamily
  where f matcher (Attribute _ body) = matcher [body]
        f _ _                        = False

(declaresMethod, declaresMethodMatching, countMethods) = deriveDeclares f :: BoundInspectionFamily
  where f matcher (Method _ equations) = matches matcher equationsExpandedExpressions $ equations
        f _       _                    = False

-- primitive can only be declared as methods
declaresPrimitive :: Operator -> Inspection
declaresPrimitive operator = containsExpression f
  where f (PrimitiveMethod o _) = operator == o
        f _                     = False
