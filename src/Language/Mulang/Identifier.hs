module Language.Mulang.Identifier (
  anyOf,
  anyone,
  except,
  like,
  named,
  Identifier,
  IdentifierPredicate) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Function (on)

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
like = on isInfixOf (map toLower)

named :: String -> IdentifierPredicate
named = (==)
