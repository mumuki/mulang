module Language.Mulang.Binding (
  named,
  like,
  anyone,
  Binding,
  BindingPredicate) where

import  Data.List (isInfixOf)

type Binding = String
type BindingPredicate = Binding -> Bool

named :: String -> BindingPredicate
named = (==)

like :: String -> BindingPredicate
like = isInfixOf

anyone :: BindingPredicate
anyone = const True
