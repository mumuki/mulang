module Text.Inflections.Tokenizer (
  CaseStyle,
  camelCase,
  snakeCase,
  rubyCase,
  canTokenize,
  tokenize) where

import Data.Char (toLower, isDigit, isLower)
import Data.Maybe (isJust)
import Data.Text (pack)

import Text.Inflections
import Text.Parsec.Error (ParseError)

import Control.Fallible

type CaseStyle = String -> Maybe [SomeWord]

camelCase      :: CaseStyle
camelCase      = orNothing . parseCamelCase [] . pack . filter (not.isDigit)

snakeCase      :: CaseStyle
snakeCase      = orNothing . parseSnakeCase [] . pack

rubyCase       :: CaseStyle
rubyCase  word | (isLower.head) word = snakeCase baseWord
               | otherwise           = camelCase baseWord

               where baseWord = filter (`notElem` "!?+-=[]<>|&*/") word

canTokenize :: CaseStyle -> String -> Bool
canTokenize style = isJust . style

tokenize :: CaseStyle -> String -> [String]
tokenize style s | Just words <- (wordsOrNothing . style) s = concatMap toToken words
                 | otherwise = []
                  where toToken = return . map toLower


wordsOrNothing = fmap (concatMap c)
                where --c (Text.Inflections.Word w) = [w]
                      c _        = []
