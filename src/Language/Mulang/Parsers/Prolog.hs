module Language.Mulang.Parsers.Prolog  (pl, parseProlog) where

import Text.Parsec
import Text.Parsec.Numbers

import Language.Mulang.Ast
import Language.Mulang.Builder
import Language.Mulang.Parsers

import Data.Maybe (maybeToList)
import Data.Char (isUpper)

import Control.Fallible
import Control.Applicative ((*>))

pl :: Parser
pl = orFail . parseProlog'

parseProlog :: MaybeParser
parseProlog = orNothing . parseProlog'

parseProlog' = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many predicate

identifier = many letter


pattern :: Parsec String a Pattern
pattern = choice [try number, try wildcard, other] <* spaces
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
  end
  return $ Fact name args

rule :: Parsec String a Expression
rule = do
  (name, args) <- phead
  def
  consults <- body
  end
  return $ Rule name args consults

phead :: Parsec String a (Identifier, [Pattern])
phead = do
  name <- identifier
  args <- rawPatternsList
  spaces
  return (name, concat.maybeToList $ args)

forall = do
  string "forall"
  startTuple
  c1 <- consult
  separator
  c2 <- consult
  endTuple
  return $ Forall c1 c2

findall = do
  string "findall"
  startTuple
  c1 <- consult
  separator
  c2 <- consult
  separator
  c3 <- consult
  endTuple
  return $ Findall c1 c2 c3

pnot = do
  string "not"
  startTuple
  c <- consult
  endTuple
  return $ Not c

exist = fmap (\(name, args) -> Exist name args) phead

inlineBody = do
  startTuple
  queries <- body
  endTuple
  return $ Sequence queries

pinfix = do
  p1 <- pattern
  spaces
  operator <- choice . map try . map string $ ["is", ">=", "=<", "\\=", ">", "<", "="]
  spaces
  p2 <- pattern
  return $ Exist operator [p1, p2]

consult = choice [try findall, try forall, try pnot, try pinfix, inlineBody, exist]

rawPatternsList = optionMaybe $ do
  startTuple
  args <- sepBy1 pattern separator
  endTuple
  return args

body = sepBy1 consult separator


def = string ":-" >> spaces
startTuple = lparen >> spaces
endTuple = rparen >> spaces
separator = comma >> spaces
end = dot >> spaces

predicate :: Parsec String a Expression
predicate = try fact <|> rule

dot = char '.'
lparen = char '('
rparen = char ')'
comma = char ','
