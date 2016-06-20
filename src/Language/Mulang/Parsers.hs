module Language.Mulang.Parsers (
  Parser,
  MaybeParser,
  EitherParser) where

import Language.Mulang

type Parser = String -> Expression
type MaybeParser = String -> Maybe Expression
type EitherParser a = String -> Either a Expression