module Language.Mulang.Inspector.Plain (
  isLValue,
  isVariable,
  isFunction,
  isClass,
  isDeclaration) where

import Language.Mulang.Ast
import Language.Mulang.Inspector.Bound (BoundInspection)

isLValue :: BoundInspection
isLValue p (LValue i _) = p i
isLValue _ _            = False

isVariable :: BoundInspection
isVariable p (Variable i _) = p i
isVariable _ _ = False

isFunction :: BoundInspection
isFunction p (Function i _) = p i
isFunction _ _              = False

isClass :: BoundInspection
isClass p (Class i _ _) = p i
isClass _ _             = False

-- TODO extract duplication in generator
isDeclaration :: BoundInspection
isDeclaration p (Attribute i _)      = p i
isDeclaration p (Class i _ _)        = p i
isDeclaration p (Clause i _ _)       = p i
isDeclaration p (Enumeration i _)    = p i
isDeclaration p (Interface i _ _)    = p i
isDeclaration p (EntryPoint i _)     = p i
isDeclaration p (Subroutine i _)     = p i
isDeclaration p (Object i _)         = p i
isDeclaration p (Clause i _ _)       = p i
isDeclaration p (Record i)           = p i
isDeclaration p (TypeAlias i _)      = p i
isDeclaration p (TypeSignature i _)  = p i
isDeclaration p (LValue i _)         = p i
isDeclaration _ _                    = False