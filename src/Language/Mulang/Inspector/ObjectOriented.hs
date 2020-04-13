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
import Language.Mulang.Generator (equationsExpandedExpressions)
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Matcher (Matcher, matches, unmatching)
import Language.Mulang.Inspector.Bound (BoundInspection, BoundCounter, containsBoundDeclaration, countBoundDeclarations, uncounting)
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
declaresObject = unmatching declaresObjectMatching

declaresObjectMatching :: Matcher -> BoundInspection
declaresObjectMatching = uncounting countObjects

countObjects :: Matcher -> BoundCounter
countObjects matcher = countBoundDeclarations f
  where f (Object _ body) = matches matcher id [body]
        f _               = False

declaresSuperclass :: BoundInspection
declaresSuperclass = inherits

declaresClass :: BoundInspection
declaresClass = unmatching declaresClassMatching

declaresClassMatching :: Matcher -> BoundInspection
declaresClassMatching = uncounting countClasses

countClasses :: Matcher -> BoundCounter
countClasses matcher = countBoundDeclarations f
  where f (Class _ _ body) = matches matcher id [body]
        f _                = False

declaresEnumeration :: BoundInspection
declaresEnumeration =  containsBoundDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

declaresInterface :: BoundInspection
declaresInterface = unmatching declaresInterfaceMatching

declaresInterfaceMatching :: Matcher -> BoundInspection
declaresInterfaceMatching = uncounting countInterfaces

countInterfaces :: Matcher -> BoundCounter
countInterfaces matcher = countBoundDeclarations f
  where f (Interface _ _ body) = matches matcher id [body]
        f _                    = False

declaresAttribute :: BoundInspection
declaresAttribute = unmatching declaresAttributeMatching

declaresAttributeMatching :: Matcher -> BoundInspection
declaresAttributeMatching = uncounting countAttributes

countAttributes :: Matcher -> BoundCounter
countAttributes matcher = countBoundDeclarations f
  where f (Attribute _ body) = matches matcher id [body]
        f _                  = False


declaresMethod :: BoundInspection
declaresMethod = unmatching declaresMethodMatching

declaresMethodMatching :: Matcher -> BoundInspection
declaresMethodMatching = uncounting countMethods

countMethods :: Matcher -> BoundCounter
countMethods matcher = countBoundDeclarations f
  where f (Method _ equations) = matches matcher equationsExpandedExpressions $ equations
        f _                    = False

-- primitive can only be declared as methods
declaresPrimitive :: Operator -> Inspection
declaresPrimitive operator = containsExpression f
  where f (PrimitiveMethod o _) = operator == o
        f _                     = False
