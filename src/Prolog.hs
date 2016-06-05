module Prolog  (pl, parseProlog) where

import Text.Parsec
import Text.Parsec.Numbers
import Text.ParserCombinators.Parsec.Prim (GenParser)

import Language.Mulang
import Language.Mulang.Builder
import Data.Either
import Data.Maybe (maybeToList)
import Data.Char (isUpper)

pl :: String -> Expression
pl string = case parseProlog string  of
          (Right v) -> v
          (Left m)  -> error.show $ m

parseProlog :: String -> Either ParseError Expression
parseProlog = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many predicate

dot = char '.'

identifier = many letter

atom :: Parsec String a Pattern
atom = fmap toPattern (many letter)
    where toPattern r
            | isUpper .head $ r = VariablePattern r
            | otherwise = LiteralPattern r

wildcard :: Parsec String a Pattern
wildcard = string "_" >> return WildcardPattern

integral :: Parsec String a Pattern
integral = fmap (LiteralPattern . show) parseIntegral

functor :: Parsec String a Pattern
functor = fmap (\(name, args) -> FunctorPattern name args) phead

pattern :: Parsec String a Pattern
pattern = choice [try integral, wildcard, try atom, functor]

fact :: Parsec String a Expression
fact = do
        (name, args) <- phead
        dot
        return $ FactDeclaration name args

rule :: Parsec String a Expression
rule = do
        (name, args) <- phead
        def
        b <- body
        dot
        return $ RuleDeclaration name args (map (\(name, args) -> Consult name args) b)

phead :: Parsec String a (Identifier, [Pattern])
phead = do
            name <- identifier
            args <- rawPatternsList
            return (name, concat.maybeToList $ args)

rawPatternsList = optionMaybe $ do
                 char '('
                 args <- sepBy1 pattern comma
                 char ')'
                 return args

body = sepBy1 phead comma

def = do
        spaces
        string ":-"
        spaces

comma = do
          spaces
          char ','
          spaces

predicate :: Parsec String a Expression
predicate = try fact <|> rule