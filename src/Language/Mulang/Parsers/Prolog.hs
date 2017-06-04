module Language.Mulang.Parsers.Prolog  (pl, parseProlog) where

import Text.Parsec
import Text.Parsec.Numbers

import Language.Mulang.Ast
import Language.Mulang.Builder
import Language.Mulang.Parsers

import Data.Maybe (maybeToList)
import Data.Char (isUpper)

import Control.Fallible

pl :: Parser
pl = orFail . parseProlog'

parseProlog :: MaybeParser
parseProlog = orNothing . parseProlog'

parseProlog' = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many predicate

dot = char '.'

identifier = many letter


pattern :: Parsec String a Pattern
pattern = choice [try number, try wildcard, other]
  where
    wildcard :: Parsec String a Pattern
    wildcard = string "_" >> return WildcardPattern

    number :: Parsec String a Pattern
    number = fmap (LiteralPattern . show) parseFloat

    other :: Parsec String a Pattern
    other = fmap otherToPattern phead

    otherToPattern (name, []) | isUpper .head $ name = VariablePattern name
                              | otherwise = LiteralPattern name
    otherToPattern (name, args) = FunctorPattern name args

fact :: Parsec String a Expression
fact = do
        (name, args) <- phead
        dot
        return $ Fact name args

rule :: Parsec String a Expression
rule = do
        (name, args) <- phead
        def
        consults <- body
        dot
        return $ Rule name args consults

phead :: Parsec String a (Identifier, [Pattern])
phead = do
            name <- identifier
            args <- rawPatternsList
            return (name, concat.maybeToList $ args)

forall = do
          string "forall"
          openParen
          c1 <- consult
          comma
          c2 <- consult
          closeParen
          return $ Forall c1 c2

findall = do
          string "findall"
          openParen
          c1 <- consult
          comma
          c2 <- consult
          comma
          c3 <- consult
          closeParen
          return $ Findall c1 c2 c3

pnot = do
          string "not"
          openParen
          c <- consult
          closeParen
          return $ Not c

exist = fmap (\(name, args) -> Exist name args) phead

pinfix = do
            p1 <- pattern
            spaces
            operator <- choice . map try . map string $ ["is", ">=", "=<", "\\=", ">", "<", "="]
            spaces
            p2 <- pattern
            return $ Exist operator [p1, p2]

consult = choice [try findall, try forall, try pnot, try pinfix, exist]

rawPatternsList = optionMaybe $ do
                 openParen
                 args <- sepBy1 pattern comma
                 closeParen
                 return args

body = sepBy1 consult comma


def = do
        spaces
        string ":-"
        spaces

openParen = char '(' >> spaces
closeParen = char ')' >> spaces
comma = spaces >> char ',' >> spaces

predicate :: Parsec String a Expression
predicate = try fact <|> rule
