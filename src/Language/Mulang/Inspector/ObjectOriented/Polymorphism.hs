{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Inspector.ObjectOriented.Polymorphism (
  usesDyamicPolymorphism,
  usesStaticPolymorphism,
  usesTemplateMethod,
  usesObjectComposition,
  usesDynamicMethodOverload,
  usesStaticMethodOverload)  where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive (Inspection)
import Language.Mulang.Inspector.Generalized (GeneralizedInspection)
import Language.Mulang.Inspector.Query (Query, inspect, select, selectCount)
import Language.Mulang.Inspector.ObjectOriented (implements, declaresMethod)
import Language.Mulang.Inspector.Typed (usesType)

import Language.Mulang.Generator (Generator, declarations, expressions)

usesDynamicMethodOverload :: Inspection
usesDynamicMethodOverload expression = inspect $ do
  klass@(Class _ _ _)                 <- declarations expression
  (SimpleMethod n1 (length -> a1) _)  <- declarations klass
  (SimpleMethod n2 (length -> a2) _)  <- declarations klass
  select (n1 == n2 && a1 /= a2)

usesStaticMethodOverload :: Inspection
usesStaticMethodOverload expression = inspect $ do
  klass@(Class _ _ _)               <- declarations expression
  s1@(SubroutineSignature n1 _ _ _) <- declarations klass
  s2@(SubroutineSignature n2 _ _ _) <- declarations klass
  select (n1 == n2 && s1 /= s2)

usesObjectComposition :: Inspection
usesObjectComposition expression = inspect $ do
  klass@(Class _ _ _)          <- declarations expression
  (Attribute name1 _)          <- declarations klass
  (Send (Reference name2) _ _) <- expressions klass
  select (name1 == name2)

usesTemplateMethod :: Inspection
usesTemplateMethod expression = inspect $ do
  klass@(Class _ _ _)          <- declarations expression
  (SimpleSend Self selector _) <- expressions klass
  select (not . declaresMethod (named selector) $ klass)

usesDyamicPolymorphism :: GeneralizedInspection
usesDyamicPolymorphism root expression = inspect $ do
  (SimpleSend _ selector _) <- expressions expression
  selectCount (>1) (methodDeclarationsOf selector root)

usesStaticPolymorphism :: GeneralizedInspection
usesStaticPolymorphism root expression = inspect $ do
  interface@(Interface interfaceId _ _) <- declarations root
  (SubroutineSignature _ _ _ _)         <- declarations interface
  select (usesType (named interfaceId) expression)
  selectCount (>1) (implementorsOf interfaceId root)

-- private

methodDeclarationsOf :: Identifier -> Generator Expression
methodDeclarationsOf selector e = do
  m@(Method s _) <- declarations e
  select (s == selector)
  return m

implementorsOf :: Identifier -> Generator Expression
implementorsOf id e = do
  m@(Class _ _ _) <- declarations e
  select (implements (named id) m)
  return m
