module Language.Mulang.Edl (
  parseQuery,
  parseExpectation,
  parseExpectations,
  parseExpectations',
  Expectation(..)) where

import           Codec.Binary.UTF8.String (encode)
import           Language.Mulang.Edl.Lexer (evalP)
import qualified Language.Mulang.Edl.Parser as P

import           Language.Mulang.Edl.Expectation (Query, Expectation(..))

parseQuery :: String -> Query
parseQuery = wrap P.parseQuery

parseExpectations :: String -> [Expectation]
parseExpectations = zipWith overrideName [0..] . wrap P.parseExpectations
  where
    overrideName :: Int -> Expectation -> Expectation
    overrideName number e@Expectation { name = ""} = e { name = "E" ++ show number }
    overrideName _      e                   = e

parseExpectations' :: String -> Either String [Expectation]
parseExpectations' = evalP P.parseExpectations . encode

parseExpectation :: String -> Expectation
parseExpectation = head . parseExpectations


wrap f = either error id . evalP f . encode
