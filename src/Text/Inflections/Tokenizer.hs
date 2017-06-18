module Text.Inflections.Tokenizer (
  CaseStyle,
  camelCase,
  snakeCase,
  tokenize) where

import Data.Char (toLower)

import Text.Inflections
import Text.Inflections.Parse.Types

import Text.SimpleParser
import Control.Fallible

type CaseStyle = MaybeParser [String]

camelCase      :: CaseStyle
camelCase      = wordsOrNothing . parseCamelCase []

snakeCase      :: CaseStyle
snakeCase      = wordsOrNothing . parseSnakeCase []

wordsOrNothing = fmap (concatMap c) . orNothing
                where c (Word w) = [w]
                      c _        = []

tokenize :: CaseStyle -> String -> [String]
tokenize parser s | Just words <- parser s = concatMap toToken words
                  | otherwise = []
                  where toToken = return . map toLower


