module Language.Explang.Expectation (
  Expectation (..),
  Scope (..),
  Flags (..),
  Count (..),
  Matcher (..),
  Binding (..),
  Predicate (..),
  noFlags,
  intransitiveFlag) where

data Expectation =
  Expectation {
    flags :: Flags,
    scope :: Scope,
    negated :: Bool,
    inspection :: String,
    binding :: Binding,
    matcher :: Matcher,
    count :: Count
  } deriving (Eq, Show)

data Scope
  = Unscoped
  | Scoped { context :: String }
  deriving (Eq, Show)

data Flags = Flags { intransitive :: Bool } deriving (Eq, Show)

data Binding
  = Any
  | Named { identifier :: String }
  | Like { identifier :: String }
  | Except { identifier :: String }
  | AnyOf { identifiers :: [String] }
  deriving (Eq, Show)

data Matcher
  = Unmatching
  | Matching { expectations :: [Predicate] }
  deriving (Eq, Show)

data Count
  = AnyCount
  | AtLeast { times :: Int }
  | AtMost { times :: Int }
  | Exactly { times :: Int }
  deriving (Eq, Show)


data Predicate
  = That { expectation :: Expectation }
  | IsNumber Double
  | IsString String
  | IsSymbol String
  | IsChar Char
  | IsTrue
  | IsFalse
  | IsNil
  | IsSelf
  | IsMath
  | IsLogic
  deriving (Eq, Show)

noFlags :: Flags
noFlags = Flags False

intransitiveFlag :: Flags
intransitiveFlag = Flags True

