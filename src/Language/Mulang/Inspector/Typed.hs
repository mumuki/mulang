module Language.Mulang.Inspector.Typed (
  declaresTypeAlias,
  declaresTypeSignature,
  typesAs,
  typesParameterAs,
  typesReturnAs,
  usesType) where

import Language.Mulang.Ast
import Language.Mulang.Inspector.Primitive (containsDeclaration)
import Language.Mulang.Inspector.Bound (BoundInspection, containsBoundDeclaration)

declaresTypeAlias :: BoundInspection
declaresTypeAlias = containsBoundDeclaration f
  where f (TypeAlias _ _) = True
        f _               = False

typesReturnAs :: BoundInspection
typesReturnAs predicate = containsDeclaration f
  where f (SubroutineSignature _ _ name _)  = predicate name
        f _                                 = False

typesParameterAs :: BoundInspection
typesParameterAs predicate = containsDeclaration f
  where f (SubroutineSignature _ names _ _)  = any predicate names
        f _                                  = False

typesAs :: BoundInspection
typesAs predicate = containsDeclaration f
  where f (VariableSignature _ name _)   = predicate name
        f _                              = False

usesType :: BoundInspection
usesType predicate = containsDeclaration f
  where f (VariableSignature _ name _)        = predicate name
        f (SubroutineSignature _ args ret _)  = any predicate (ret:args)
        f _                                   = False

declaresTypeSignature :: BoundInspection
declaresTypeSignature = containsBoundDeclaration f
  where f (TypeSignature _ _) = True
        f _                   = False
