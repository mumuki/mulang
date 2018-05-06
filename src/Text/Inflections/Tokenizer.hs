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
import Text.Inflections (SomeWord, unSomeWord)
import Data.Text (pack, unpack)

import Control.Fallible

type CaseStyle = String -> Either String [SomeWord]

camelCase      :: CaseStyle
camelCase      = first show . parseCamelCase [] . pack . filter (not.isDigit)

snakeCase      :: CaseStyle
snakeCase      = first show . parseSnakeCase [] . pack

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

wordsOrNothing = fmap (map c) . orNothing
                where c t        = unpack (unSomeWord id t)
