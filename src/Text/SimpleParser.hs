module Text.SimpleParser (
  SimpleParser,
  MaybeParser,
  EitherParser) where

type SimpleParser a = String -> a
type MaybeParser a = String -> Maybe a
type EitherParser a = String -> Either String a