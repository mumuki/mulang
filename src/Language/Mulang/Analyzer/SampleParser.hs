module        Language.Mulang.Analyzer.SampleParser (
  parseSample) where

import        Language.Mulang
import        Language.Mulang.Parsers (EitherParser, maybeToEither)
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Java (parseJava)
import        Language.Mulang.Analyzer.Analysis (Sample(..), Language(..))

parseSample :: Sample -> Either String Expression
parseSample (CodeSample language content) = (parserFor language) content
parseSample (MulangSample ast)            = Right ast

parserFor :: Language -> EitherParser
parserFor Haskell        = parseHaskell
parserFor Java           = parseJava
parserFor JavaScript     = maybeToEither parseJavaScript
parserFor Prolog         = error "unsupported"
parserFor Python         = error "unsupported"
