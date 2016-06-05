module Prolog where

import Text.Parsec
import Language.Mulang
import Language.Mulang.Builder
import Data.Either
import Data.Maybe (maybeToList)
import Data.Char (isUpper)

pl :: String -> Expression
pl string | (Right v) <- parseProlog string = v

parseProlog :: String -> Either ParseError Expression
parseProlog = fmap compact . parse program ""

program :: Parsec String a [Expression]
program = many predicate

dot = char '.'

atom = many letter

fact = do
        (name, args) <- functor
        dot
        return $ FactDeclaration name (map rawPatternToPattern args)

rule = do
        (name, args) <- functor
        def
        _ <- body
        dot
        return $ RuleDeclaration name (map rawPatternToPattern args) []

functor = do
            name <- atom
            args <- rawPatternsList
            return (name, concat.maybeToList $ args)

rawPatternsList = optionMaybe $ do
                 char '('
                 args <- sepBy1 atom comma
                 char ')'
                 return args

rawPatternToPattern r
        | isUpper .head $ r = VariablePattern r
        | otherwise = LiteralPattern r

body = sepBy1 functor comma

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