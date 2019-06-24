module Language.Mulang.Identifier (
  anyone,
  named,
  except,
  like,
  unlike,
  anyOf,
  noneOf,
  likeAnyOf,
  likeNoneOf,
  Identifier,
  IdentifierPredicate) where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Function (on)

-- | An identifier
-- | Mulang does not assume any special naming convention or format
type Identifier = String
type IdentifierPredicate = Identifier -> Bool

anyone :: IdentifierPredicate
anyone = const True

named :: String -> IdentifierPredicate
named = (==)

except :: String -> IdentifierPredicate
except = (/=)

like :: String -> IdentifierPredicate
like = on isInfixOf (map toLower)

unlike :: String -> IdentifierPredicate
unlike identifier = not . like identifier

anyOf :: [Identifier] -> IdentifierPredicate
anyOf options identifier = any (== identifier) options

noneOf :: [Identifier] -> IdentifierPredicate
noneOf options = not . anyOf options

likeAnyOf :: [Identifier] -> IdentifierPredicate
likeAnyOf options identifier = any (`like` identifier) options

likeNoneOf :: [Identifier] -> IdentifierPredicate
likeNoneOf options = not . likeAnyOf options
