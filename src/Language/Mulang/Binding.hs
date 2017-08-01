module Language.Mulang.Binding (
  andAlso,
  anyOf,
  anyone,
  except,
  like,
  named,
  Binding,
  BindingPredicate) where

import  Data.List (isInfixOf)

type Binding = String
type BindingPredicate = Binding -> Bool

andAlso :: BindingPredicate -> BindingPredicate -> BindingPredicate
andAlso p1 p2 binding = p1 binding && p2 binding

anyOf :: [Binding] -> BindingPredicate
anyOf options binding = any (== binding) options

anyone :: BindingPredicate
anyone = const True

except :: String -> BindingPredicate
except = (/=)

like :: String -> BindingPredicate
like = isInfixOf

named :: String -> BindingPredicate
named = (==)
