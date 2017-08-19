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

  declaresMethod)  where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive (
  Inspection, IdentifierInspection,
  containsExpression, containsDeclaration, containsBoundDeclaration)

import Control.Monad (MonadPlus, guard)
import Language.Mulang.Generator (declarations, expressions)

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

exists :: [a] -> Bool
exists = not.null

guardCount :: MonadPlus m => (Int -> Bool) -> [a] -> m ()
guardCount condition list = guard (condition . length $ list)

usedSelectors :: Expression -> [Identifier]
usedSelectors e = do
  (Send _ (Reference m) _) <- expressions e
  return m

methodDeclarationsOf :: Identifier -> Expression -> [Expression]
methodDeclarationsOf selector e = do
  m@(Method s _) <- declarations e
  guard (s == selector)
  return m

usesDyamicPolymorphism :: Inspection
usesDyamicPolymorphism expression = exists $ do
  selector <- usedSelectors expression
  guardCount (>1) (methodDeclarationsOf selector expression)

-- usesStaticPolymorphism :: Inspection
-- usesStaticPolymorphism expression = exists $ do
--   interface <- staticInterfaces expression
--   _ <- usersOf interface expression
--   guardCount (>1) (implementorsOf interface expression)

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
