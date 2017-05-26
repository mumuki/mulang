module Language.Mulang.Parsers
  (Parser, MaybeParser) where

import           Language.Mulang.Ast
import qualified Text.SimpleParser as S (SimpleParser, MaybeParser)

type Parser = S.SimpleParser Expression
type MaybeParser = S.MaybeParser Expression