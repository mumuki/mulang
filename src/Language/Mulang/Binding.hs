module Language.Mulang.Binding (
  named,
  like,
  anyone,
  except,
  andAlso,
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

except :: String -> BindingPredicate
except = (/=)

andAlso :: BindingPredicate -> BindingPredicate -> BindingPredicate
andAlso p1 p2 binding = p1 binding && p2 binding
