module        Language.Mulang.Analyzer.SampleParser (
  parseSample) where

import        Language.Mulang
import        Language.Mulang.Parsers (MaybeParser)
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Prolog (parseProlog)
import        Language.Mulang.Parsers.Java (parseJava)
import        Language.Mulang.Analyzer.Analysis (Sample(..), Language(..))

parseSample :: Sample -> Maybe Expression
parseSample (CodeSample language content) = (parserFor language) content
parseSample (MulangSample ast)            = Just ast

parserFor :: Language -> MaybeParser
parserFor Haskell        = parseHaskell
parserFor Java           = parseJava
parserFor JavaScript     = parseJavaScript
parserFor Prolog         = parseProlog
