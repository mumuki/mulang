module Language.Mulang.Identifier (
  andAlso,
  anyOf,
  anyone,
  except,
  like,
  named,
  Identifier,
  IdentifierPredicate,
  PredicateModifier) where

import  Data.List (isInfixOf)

-- | An identifier
-- | Mulang does not assume any special naming convention or format
type Identifier = String
type IdentifierPredicate = Identifier -> Bool
type PredicateModifier = IdentifierPredicate -> IdentifierPredicate

andAlso :: IdentifierPredicate -> PredicateModifier
andAlso p1 p2 identifier = p1 identifier && p2 identifier

anyOf :: [Identifier] -> IdentifierPredicate
anyOf options identifier = any (== identifier) options

anyone :: IdentifierPredicate
anyone = const True

except :: String -> IdentifierPredicate
except = (/=)

like :: String -> IdentifierPredicate
like = isInfixOf

named :: String -> IdentifierPredicate
named = (==)
