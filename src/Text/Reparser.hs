{-# LANGUAGE RankNTypes #-}

module Text.Reparser (
  reparse,
  SyntaxError (..),
  SyntaxErrorKind (..)) where

import Text.Parsec
import Text.SimpleParser (ParsecParser)
import Control.Fallible (orFail)
import Control.Monad (msum, join)
import Data.Maybe (isJust)

data SyntaxError
  = SyntaxError { line :: Int, column :: Int, kind :: SyntaxErrorKind } deriving (Show, Eq)

type Reparser = ParsecParser (Maybe SyntaxError)

data SyntaxErrorKind
  = UnclosedBracket
  | UnclosedParen
  | UnclosedBrace
  | UnopenBracket
  | UnopenParen
  | UnopenBrace
  | BrokenQuote
  | BrokenDoubleQuote
  | UnopenComment
  | UnclosedComment
  | NonAsciiChar
  deriving (Show, Eq)

lparen, rparen,
  rbrace, lbrace,
  lbracket, rbracket,
  quote, doubleQuote :: ParsecParser Char

lparen = char '('
rparen = char ')'
lbrace = char '{'
rbrace = char '}'
lbracket = char '['
rbracket = char ']'
quote = char '\''
doubleQuote = char '"'


reparse :: String -> Maybe SyntaxError
reparse = orFail . parse program ""

program :: Reparser
program = strip False

strip :: Bool -> Reparser
strip nested = fmap msum $ many (choice choices)
  where choices | nested = commonParsers
                | otherwise = commonParsers ++ withModes unopen

        commonParsers = stream : quotation : doubleQuotation : withModes open
        withModes f = map f [bracketMode, parenMode, braceMode]

data Mode = Mode {
  leftParser :: ParsecParser Char,
  rightParser :: ParsecParser Char,
  unopenKind :: SyntaxErrorKind,
  unclosedKind :: SyntaxErrorKind
}

parenMode   = Mode lparen rparen UnopenParen UnclosedParen
bracketMode = Mode lbracket rbracket UnopenBracket UnclosedBracket
braceMode   = Mode lbrace rbrace UnopenBrace UnclosedBrace

quotation :: Reparser
quotation = quote *> many (noneOf "'") *> quote *> return Nothing

doubleQuotation :: Reparser
doubleQuotation = doubleQuote *> many (noneOf "\"") *> doubleQuote *> return Nothing


open :: Mode -> Reparser
open mode = do
  position <- getPosition
  nested <- startNesting mode
  if isJust nested
    then return nested
    else endNesting mode position

unopen :: Mode -> Reparser
unopen mode = do
  position <- getPosition
  rightParser mode
  returnSyntaxError position (unopenKind mode)

startNesting mode = leftParser mode *> (fmap join . optionMaybe $ strip True)

endNesting mode position = closed mode <|> unclosed mode position

closed mode = rightParser mode *> return Nothing

unclosed mode position = returnSyntaxError position (unclosedKind mode)

stream :: Reparser
stream = many1 (noneOf "'\"{}[]()") *> return Nothing

returnSyntaxError position = return . Just . SyntaxError (sourceLine position) (sourceColumn position)
