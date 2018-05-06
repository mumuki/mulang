module Text.Inflections.Tokenizer (
  CaseStyle,
  camelCase,
  snakeCase,
  rubyCase,
  canTokenize,
  tokenize) where

import Data.Char (toLower, isDigit, isLower)
import Data.Either (isRight)
import Data.Bifunctor (first)

import Text.Inflections (parseCamelCase, parseSnakeCase)
import Text.Inflections.Parse.Types

import Control.Fallible

type CaseStyle = String -> Either String [Text.Inflections.Parse.Types.Word]

camelCase      :: CaseStyle
camelCase      = first show . parseCamelCase [] . filter (not.isDigit)

snakeCase      :: CaseStyle
snakeCase      = first show . parseSnakeCase []

rubyCase       :: CaseStyle
rubyCase  word | (isLower.head) word = snakeCase baseWord
               | otherwise           = camelCase baseWord

               where baseWord = filter (`notElem` "!?+-=[]<>|&*/") word

canTokenize :: CaseStyle -> String -> Bool
canTokenize style = isRight . style

tokenize :: CaseStyle -> String -> [String]
tokenize style s | Just words <- (wordsOrNothing . style) s = concatMap toToken words
                 | otherwise = []
                  where toToken = return . map toLower


wordsOrNothing = fmap (concatMap c) . orNothing
                where c (Word w) = [w]
                      c _        = []
