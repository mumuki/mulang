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

-- | true for all identifiers
anyone :: IdentifierPredicate
anyone = const True

-- | true for only the given identifier
named :: String -> IdentifierPredicate
named = (==)

-- true for any identifier different to the given one
except :: String -> IdentifierPredicate
except = (/=)

-- | true for any identifier that contains the given one, ignoring case
like :: String -> IdentifierPredicate
like = on isInfixOf (map toLower)

-- | false for any identifier that contains the given one, ignoring case
unlike :: String -> IdentifierPredicate
unlike identifier = not . like identifier

-- | true for any identifier that is equal to any of the given ones
anyOf :: [Identifier] -> IdentifierPredicate
anyOf options identifier = any (== identifier) options

-- | true for any identifier that is different to all of the given ones
noneOf :: [Identifier] -> IdentifierPredicate
noneOf options = not . anyOf options

-- | true for any identifier that is like any of the given ones
likeAnyOf :: [Identifier] -> IdentifierPredicate
likeAnyOf options identifier = any (`like` identifier) options

-- | true for any identifier that is not like any of the given ones
likeNoneOf :: [Identifier] -> IdentifierPredicate
likeNoneOf options = not . likeAnyOf options
