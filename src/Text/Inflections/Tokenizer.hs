module Text.Inflections.Tokenizer (
  camelCase, snakeCase,
  tokenize) where

import Data.Char (toLower)

import Text.Inflections
import Text.Inflections.Parse.Types

import Text.SimpleParser
import Control.Fallible

camelCase      :: MaybeParser [String]
camelCase      = wordsOrNothing . parseCamelCase []

snakeCase      :: MaybeParser [String]
snakeCase      = wordsOrNothing . parseSnakeCase []

wordsOrNothing = fmap (concatMap c). orNothing
                where c (Word w) = [w]
                      c _        = []

tokenize :: MaybeParser [String] -> String -> [String]
tokenize parser s | Just words <- parser s = concatMap toToken words
                  | otherwise = []
                  where toToken = return . map toLower


