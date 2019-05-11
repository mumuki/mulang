module Language.Mulang.Identifier (
  anyOf,
  anyone,
  except,
  like,
  named,
  Identifier,
  IdentifierPredicate) where

import  Data.List (isInfixOf)

-- | An identifier
-- | Mulang does not assume any special naming convention or format
type Identifier = String
type IdentifierPredicate = Identifier -> Bool

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
