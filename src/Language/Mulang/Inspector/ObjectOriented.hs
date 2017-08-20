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
  usesDyamicPolymorphism,
  usesStaticPolymorphism,
  usesTemplateMethod,
  usesObjectComposition,
  declaresMethod)  where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive (
  Inspection, IdentifierInspection,
  containsExpression, containsDeclaration, containsBoundDeclaration)
import Language.Mulang.Inspector.Typed (typesAs)

import Control.Monad (MonadPlus, guard)
import Language.Mulang.Generator (Generator, declarations, expressions)

implements :: IdentifierInspection
implements predicate = containsExpression f
  where f (Implement (Reference name)) = predicate name
        f _                            = False

includes :: IdentifierInspection
includes predicate = containsExpression f
  where f (Include (Reference name)) = predicate name
        f _                          = False

inherits :: IdentifierInspection
inherits predicate = containsDeclaration f
  where f (Class _ (Just name) _) = predicate name
        f _                       = False

instantiates :: IdentifierInspection
instantiates predicate = containsExpression f
  where f (New (Reference name) _) = predicate name
        f _                        = False

inspect :: [a] -> Bool
inspect = not.null

guardCount :: MonadPlus m => (Int -> Bool) -> [a] -> m ()
guardCount condition list = guard (condition . length $ list)

methodDeclarationsOf :: Identifier -> Generator Expression
methodDeclarationsOf selector e = do
  m@(Method s _) <- declarations e
  guard (s == selector)
  return m

implementorsOf :: Identifier -> Generator Expression
implementorsOf id e = do
  m@(Class _ _ _) <- declarations e
  guard (implements (named id) m)
  return m

usesObjectComposition :: Inspection
usesObjectComposition expression = inspect $ do
  klass@(Class _ _ _)          <- declarations expression
  (Attribute name1 _)          <- declarations klass
  (Send (Reference name2) _ _) <- expressions klass
  guard (name1 == name2)

usesTemplateMethod :: Inspection
usesTemplateMethod expression = inspect $ do
  klass@(Class _ _ _)          <- declarations expression
  (SimpleSend Self selector _) <- expressions klass
  guard (not . declaresMethod (named selector) $ klass)

usesDyamicPolymorphism :: Inspection
usesDyamicPolymorphism expression = inspect $ do
  (Send _ (Reference selector) _) <- expressions expression
  guardCount (>1) (methodDeclarationsOf selector expression)

usesStaticPolymorphism :: Inspection
usesStaticPolymorphism expression = inspect $ do
  (Interface interfaceId _ _) <- declarations expression
  guard (typesAs (named interfaceId) expression)
  guardCount (>1) (implementorsOf interfaceId expression)

{-
usesStaticStrategy :: Inspection
usesStaticStrategy expression = exists $ do
  interface@(Interface _ _)    <- declarations expression
  guardCount (>1) (implementorsOf interface expression)

  klass@(Class _ _ _)          <- declarations expression
  attribute@(Attribute name1 _) <- declarations klass

  guard (typedAs interface attribute)

  (Send (Reference name2) _ _) <- expressions klass
  guard (name1 == name2)
-}

usesInheritance :: Inspection
usesInheritance = declaresSuperclass anyone

usesMixins :: Inspection
usesMixins = includes anyone

declaresObject :: IdentifierInspection
declaresObject =  containsBoundDeclaration f
  where f (Object _ _) = True
        f _            = False

declaresSuperclass :: IdentifierInspection
declaresSuperclass = inherits

declaresClass :: IdentifierInspection
declaresClass =  containsBoundDeclaration f
  where f (Class _ _ _) = True
        f _             = False

declaresEnumeration :: IdentifierInspection
declaresEnumeration =  containsBoundDeclaration f
  where f (Enumeration _ _) = True
        f _                 = False

declaresInterface :: IdentifierInspection
declaresInterface =  containsBoundDeclaration f
  where f (Interface _ _ _) = True
        f _                 = False

declaresAttribute :: IdentifierInspection
declaresAttribute =  containsBoundDeclaration f
  where f (Attribute _ _) = True
        f _               = False

declaresMethod :: IdentifierInspection
declaresMethod =  containsBoundDeclaration f
  where f (Method _ _) = True
        f _            = False
