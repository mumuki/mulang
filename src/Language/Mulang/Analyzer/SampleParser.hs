module        Language.Mulang.Analyzer.SampleParser (
  parseSample) where

import        Language.Mulang
import        Language.Mulang.Parsers (EitherParser, maybeToEither)
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Prolog (parseProlog)
import        Language.Mulang.Parsers.Java (parseJava)
import        Language.Mulang.Parsers.Python (parsePython)
import        Language.Mulang.Analyzer.Analysis (Sample(..), Language(..))
import        Language.Mulang.Builder (normalize, normalizeWith, NormalizationOptions)

parseSample :: Sample -> Either String Expression
parseSample (CodeSample language content) = (parserFor language) content
parseSample (MulangSample ast options)    = Right . (normalizerFor options) $ ast

parserFor :: Language -> EitherParser
parserFor Haskell        = parseHaskell
parserFor Java           = parseJava
parserFor JavaScript     = maybeToEither parseJavaScript
parserFor Prolog         = parseProlog
parserFor Python         = parsePython

normalizerFor :: (Maybe NormalizationOptions) -> (Expression -> Expression)
normalizerFor Nothing        = normalize
normalizerFor (Just options) = normalizeWith options
