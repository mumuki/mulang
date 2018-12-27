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
import Language.Mulang.Inspector.Primitive (Inspection, InspectionContext, inspect)
import Language.Mulang.Inspector.ObjectOriented (implements, declaresMethod)
import Language.Mulang.Inspector.Typed (usesType)

import Control.Monad (guard)
import Language.Mulang.Generator (Generator, declarations, expressions)

usesDynamicMethodOverload :: Inspection
usesDynamicMethodOverload expression = inspect $ do
  klass@(Class _ _ _)                 <- declarations expression
  (SimpleMethod n1 (length -> a1) _)  <- declarations klass
  (SimpleMethod n2 (length -> a2) _)  <- declarations klass
  guard (n1 == n2 && a1 /= a2)

usesStaticMethodOverload :: Inspection
usesStaticMethodOverload expression = inspect $ do
  klass@(Class _ _ _)               <- declarations expression
  s1@(SubroutineSignature n1 _ _ _) <- declarations klass
  s2@(SubroutineSignature n2 _ _ _) <- declarations klass
  guard (n1 == n2 && s1 /= s2)

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

-- uncontextualizable
usesDyamicPolymorphism :: Inspection
usesDyamicPolymorphism expression = inspect $ do
  (SimpleSend _ selector _) <- expressions expression
  guardCount (>1) (methodDeclarationsOf selector expression)

-- uncontextualizable
usesStaticPolymorphism :: Inspection
usesStaticPolymorphism expression = inspect $ do
  interface@(Interface interfaceId _ _) <- declarations expression
  (SubroutineSignature _ _ _ _)         <- declarations interface
  guard (usesType (named interfaceId) expression)
  guardCount (>1) (implementorsOf interfaceId expression)

-- private

guardCount :: (Int -> Bool) -> [Expression] -> InspectionContext
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
