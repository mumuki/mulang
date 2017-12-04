{-# LANGUAGE RankNTypes #-}

module Text.SimpleParser (
  SimpleParser,
  MaybeParser,
  EitherParser,
  ParsecParser) where

import Text.Parsec (Parsec)

type SimpleParser a = String -> a
type MaybeParser a = String -> Maybe a
type EitherParser a = String -> Either String a
type ParsecParser a = forall x . Parsec String x a
