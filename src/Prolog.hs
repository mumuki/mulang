module Prolog where

import Text.Parsec
import Language.Mulang

pl string | (Right v) <- parseProlog string = v

parseProlog = parse program ""

program :: Parsec String a [Expression]
program = many predicate

dot = char '.'

atom = many letter

fact = do
        (name, args) <- functor
        dot
        return $ FactDeclaration name

rule = do
        (name, args) <- functor
        string ":-"
        _ <- body
        dot
        return $ RuleDeclaration name

functor = do
          name <- atom
          _ <- optionMaybe $ do
                         char '('
                         arg <- atom
                         char ')'
          return (name, [])

body = functor

predicate :: Parsec String a Expression
predicate = try fact <|> rule