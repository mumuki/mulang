{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Edl.Expectation (
  cQuery,
  Expectation (..),
  Matcher (..),
  Predicate (..),
  Clause (..),
  Query (..),
  CQuery (..),
  TQuery (..)) where

import GHC.Generics

data Expectation =
  Expectation {
    name :: String,
    query :: Query
  } deriving (Eq, Show, Generic)

data Query
  = Decontextualize CQuery
  | Within String CQuery
  | Through String CQuery
  | Not Query
  | And Query Query
  | Or Query Query
  deriving (Eq, Show, Generic)

data CQuery
  = Inspection String Predicate Matcher
  | CNot CQuery
  | CAnd CQuery CQuery
  | COr CQuery CQuery
  | AtLeast Int TQuery
  | AtMost Int TQuery
  | Exactly Int TQuery
  deriving (Eq, Show, Generic)

data TQuery
  = Plus TQuery TQuery
  | Counter String Predicate Matcher
  deriving (Eq, Show, Generic)

data Predicate
  = Any
  | Named String
  | Like String
  | Unlike String
  | Except String
  | AnyOf [String]
  | NoneOf [String]
  | LikeAnyOf [String]
  | LikeNoneOf [String]
  deriving (Eq, Show, Generic)

data Matcher
  = Unmatching
  | Matching [Clause]
  deriving (Eq, Show, Generic)

data Clause
  = That Query
  | IsAnything
  | IsChar Char
  | IsFalse
  | IsLiteral
  | IsLogic
  | IsMath
  | IsNil
  | IsNonliteral
  | IsAnyString
  | IsAnyNumber
  | IsNumber Double
  | IsReference String
  | IsSelf
  | IsString String
  | IsSymbol String
  | IsTrue
  deriving (Eq, Show, Generic)

cQuery :: Query -> Maybe CQuery
cQuery (Decontextualize q) = Just q
cQuery (Within _ q)        = Just q
cQuery (Through _ q)       = Just q
cQuery _                   = Nothing
