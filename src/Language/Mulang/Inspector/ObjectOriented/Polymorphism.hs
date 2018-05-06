module Language.Mulang.Inspector.ObjectOriented.Polymorphism (
  usesDyamicPolymorphism,
  usesStaticPolymorphism,
  usesTemplateMethod,
  usesObjectComposition)  where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive (Inspection)
import Language.Mulang.Inspector.ObjectOriented (implements, declaresMethod)
import Language.Mulang.Inspector.Typed (typesAs)

import Control.Monad (MonadPlus, guard)
import Language.Mulang.Generator (Generator, declarations, expressions)

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

-- private

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
