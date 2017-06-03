module        Language.Mulang.Analyzer.CodeSampleParser (
  parseCodeSample) where

import        Language.Mulang
import        Language.Mulang.Parsers (MaybeParser)
import        Language.Mulang.Parsers.Json
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Prolog (parseProlog)
import        Language.Mulang.Parsers.Gobstones (parseGobstones, parseGobstonesAst)
import        Language.Mulang.Analyzer.Analysis (CodeSample(..), Language(..))

import        Text.Read

parseCodeSample :: CodeSample -> Maybe Expression
parseCodeSample (CodeSample language content)         = (parserFor language) content

parserFor :: Language -> MaybeParser
parserFor Mulang         = readMaybe
parserFor Json           = parseJson
parserFor Haskell        = parseHaskell
parserFor JavaScript     = parseJavaScript
parserFor Prolog         = parseProlog
parserFor Gobstones      = parseGobstones
parserFor GobstonesAst   = parseGobstonesAst