module Language.Mulang.Inspector.Typed (
  declaresTypeAlias,
  declaresTypeSignature,
  typesAs,
  typesParameterAs,
  typesReturnAs,
  usesType) where

import Language.Mulang.Ast
import Language.Mulang.Identifier
import Language.Mulang.Inspector.Primitive (IdentifierInspection, containsDeclaration, containsBoundDeclaration)

declaresTypeAlias :: IdentifierInspection
declaresTypeAlias = containsBoundDeclaration f
  where f (TypeAlias _ _) = True
        f _               = False

typesReturnAs :: IdentifierInspection
typesReturnAs predicate = containsDeclaration f
  where f (SubroutineSignature _ _ name _)  = predicate name
        f _                                 = False

typesParameterAs :: IdentifierInspection
typesParameterAs predicate = containsDeclaration f
  where f (SubroutineSignature _ names _ _)  = any predicate names
        f _                                  = False

typesAs :: IdentifierInspection
typesAs predicate = containsDeclaration f
  where f (VariableSignature _ name _)   = predicate name
        f _                              = False

usesType :: IdentifierInspection
usesType predicate = containsDeclaration f
  where f (VariableSignature _ name _)        = predicate name
        f (SubroutineSignature _ args ret _)  = any predicate (ret:args)
        f _                                   = False

declaresTypeSignature :: IdentifierInspection
declaresTypeSignature = containsBoundDeclaration f
  where f (TypeSignature _ _) = True
        f _                   = False
