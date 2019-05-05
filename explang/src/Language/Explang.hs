module Language.Explang (
  parseExpectation,
  module Language.Explang.Expectation) where

import Codec.Binary.UTF8.String (encode)
import Language.Explang.Parser (parse)
import Language.Explang.Lexer (evalP)

import Language.Explang.Expectation

parseExpectation :: String -> Either String Expectation
parseExpectation = evalP parse . encode

