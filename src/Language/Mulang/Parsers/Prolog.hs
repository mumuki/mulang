{-# LANGUAGE RankNTypes #-}

module Language.Mulang.Parsers.Prolog  (pl, parseProlog) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Numbers
import Text.SimpleParser (ParsecParser)

import Language.Mulang.Ast
import Language.Mulang.Builder
import Language.Mulang.Parsers

import Data.Maybe (fromMaybe)
import Data.Char (isUpper)

import Control.Fallible

pl :: Parser
pl = orFail . parseProlog'

parseProlog :: EitherParser
parseProlog = orLeft . parseProlog'

parseProlog' = fmap compact . parse program ""

program :: ParsecParser [Expression]
program = many predicate

pattern :: ParsecParser Pattern
pattern = buildExpressionParser optable (term <* spaces)
  where
    optable = [ [ Infix (op "^") AssocLeft ],
                [ Infix (op "*") AssocLeft ],
                [ Infix (op "/") AssocLeft ],
                [ Infix (op "+") AssocLeft ],
                [ Infix (op "-") AssocLeft ] ]
    term    = try number <|> wildcard <|> tuple <|> (fmap otherToPattern phead)

    otherToPattern (name, []) | isUpper . head $ name = VariablePattern name
                              | otherwise = LiteralPattern name
    otherToPattern ("abs", [p])           = ApplicationPattern "abs" [p]
    otherToPattern ("mod", [p1, p2])      = ApplicationPattern "mod" [p1, p2]
    otherToPattern ("div", [p1, p2])      = ApplicationPattern "div" [p1, p2]
    otherToPattern ("rem", [p1, p2])      = ApplicationPattern "rem" [p1, p2]
    otherToPattern (name, args)           = FunctorPattern name args

op :: String -> ParsecParser (Pattern -> Pattern -> Pattern)
op symbol = string symbol <* spaces >> return (\x y -> ApplicationPattern symbol [x, y])

tuple :: ParsecParser Pattern
tuple = fmap compact patternsList
  where compact [p] = p
        compact ps  = TuplePattern ps

wildcard :: ParsecParser Pattern
wildcard = string "_" >> return WildcardPattern

number :: ParsecParser Pattern
number = fmap (LiteralPattern . show) parseFloat

fact :: ParsecParser Expression
fact = do
  (name, args) <- phead
  end
  return $ Fact name args

rule :: ParsecParser Expression
rule = do
  (name, args) <- phead
  def
  consults <- body
  end
  return $ Rule name args consults

phead :: ParsecParser (Identifier, [Pattern])
phead = do
  name <- identifier
  args <- optionMaybe patternsList
  spaces
  return (name, fromMaybe [] args)

forall :: ParsecParser Expression
forall = do
  string "forall"
  startTuple
  c1 <- consult
  separator
  c2 <- consult
  endTuple
  return $ Forall c1 c2

findall :: ParsecParser Expression
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

cut :: ParsecParser Expression
cut = do
  bang
  return $ Exist "!" []

pnot :: ParsecParser Expression
pnot = do
  c <- operator <|> functor
  return $ Not c
  where
    functor = string "not" >> startTuple >> consult <* endTuple
    operator = negation >> consult

exist :: ParsecParser Expression
exist = fmap (\(name, args) -> Exist name args) phead

body :: ParsecParser [Expression]
body =  sepBy1 consult separator

inlineBody :: ParsecParser Expression
inlineBody = do
  startTuple
  queries <- body
  endTuple
  return $ Sequence queries

pinfix :: ParsecParser Expression
pinfix = do
  p1 <- pattern
  spaces
  operator <- choice . map try . map string $ ["is", "|", ">=", "=<", "\\=", ">", "<", "=:=", "==", "=\\=", ":=", "=..", "=@=", "\\\\=@=", "="]
  spaces
  p2 <- pattern
  return $ Exist operator [p1, p2]


consult :: ParsecParser Expression
consult = choice [try findall, try forall, try pnot, try pinfix, inlineBody, cut, exist]

predicate :: ParsecParser Expression
predicate = try fact <|> rule

patternsList :: ParsecParser [Pattern]
patternsList = startTuple >> sepBy1 pattern separator <* endTuple

bang = string "!" >> spaces
negation = string "\\+" >> spaces
identifier = many1 letter
def = string ":-" >> spaces
startTuple = lparen >> spaces
endTuple = rparen >> spaces
separator = (char ',' <|> char ';') >> spaces
end = char '.' >> spaces

lparen = char '('
rparen = char ')'
