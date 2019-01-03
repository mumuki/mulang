module Language.Mulang.Inspector.Bound (
  bind,
  containsBoundDeclaration,
  BoundInspection) where

import  Language.Mulang.Inspector.Primitive (Inspection)
import  Language.Mulang.Identifier (IdentifierPredicate)
import  Language.Mulang.Ast (Expression)
import  Language.Mulang.Generator (boundDeclarations)
import  Data.List.Extra(has)

type BoundInspection = IdentifierPredicate -> Inspection

bind :: Inspection -> BoundInspection
bind = const

containsBoundDeclaration :: (Expression -> Bool) -> BoundInspection
containsBoundDeclaration f b  = has f (boundDeclarations b)
