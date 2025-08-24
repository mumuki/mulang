module Text.Inflections.Tokenizer (
  CaseStyle,
  camelCase,
  snakeCase,
  rubyCase,
  canTokenize,
  tokenize) where

import Data.Text (Text, pack, unpack)
import Data.Char (toLower, isDigit, isLower)
import Data.Either (isRight)
import Data.Void (Void)

import Text.Megaparsec.Error (ParseErrorBundle)

import Text.Inflections
import Text.Parsec.Error (ParseError)

import Control.Fallible

type CaseStyle = String -> Either (ParseErrorBundle Text Void) [SomeWord]

camelCase      :: CaseStyle
camelCase      = parseCamelCase [] . pack . filter (not.isDigit)

snakeCase      :: CaseStyle
snakeCase      = parseSnakeCase [] . pack

rubyCase       :: CaseStyle
rubyCase word@(i:_) | i == '_' = snakeCase . unprivatize $ baseWord
                    | isLower i = snakeCase baseWord
                    | otherwise = camelCase baseWord

                    where

                      baseWord = filter (`notElem` "!?+-=[]<>|&*/") word

                      unprivatize = unprivatizeRight . unprivatizeLeft

                      unprivatizeLeft ('_':'_':xs) = xs
                      unprivatizeLeft ('_':xs)     = xs
                      unprivatizeLeft xs           = xs

                      unprivatizeRight = reverse .  unprivatizeLeft . reverse

canTokenize :: CaseStyle -> String -> Bool
canTokenize style = isRight . style

tokenize :: CaseStyle -> String -> [String]
tokenize style s | Just words <- (wordsOrNothing . style) s = concatMap toToken words
                 | otherwise = []
                  where toToken = return . map toLower

wordsOrNothing :: Either (ParseErrorBundle Text Void) [SomeWord] -> Maybe [String]
wordsOrNothing = fmap (concatMap c ) . orNothing
                where c (SomeWord w) = [unpack . unWord $ w]
                      c _        = []
