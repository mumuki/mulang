module Prolog where

import Text.Parsec
import Language.Mulang
import Language.Mulang.Builder
import Data.Either

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
        return $ FactDeclaration name []

rule = do
        (name, args) <- functor
        def
        _ <- body
        dot
        return $ RuleDeclaration name [] []

functor = do
            name <- atom
            _ <- optionMaybe $ do
                           char '('
                           args <- sepBy1 atom comma
                           char ')'
            return (name, [])

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