module Language.Mulang.Parsers
  (Parser, EitherParser, MaybeParser, maybeToEither) where

import           Language.Mulang.Ast
import qualified Text.SimpleParser as S (SimpleParser, EitherParser, MaybeParser)

type Parser = S.SimpleParser Expression
type EitherParser = S.EitherParser Expression
type MaybeParser = S.MaybeParser Expression

maybeToEither :: MaybeParser -> EitherParser
maybeToEither parser input | (Just result) <- parser input = Right result
                           | otherwise = Left "Sample code parsing error"
