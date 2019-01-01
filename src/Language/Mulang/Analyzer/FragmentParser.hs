module        Language.Mulang.Analyzer.FragmentParser (
  parseFragment) where

import        Language.Mulang
import        Language.Mulang.Parsers (EitherParser, maybeToEither)
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Prolog (parseProlog)
import        Language.Mulang.Parsers.Java (parseJava)
import        Language.Mulang.Parsers.Python (parsePython)
import        Language.Mulang.Analyzer.Analysis (Fragment(..), Language(..))
import        Language.Mulang.Builder (normalize, normalizeWith, NormalizationOptions)

parseFragment :: Fragment -> Either String Expression
parseFragment (CodeFragment language content) = (parserFor language) content
parseFragment (MulangFragment ast options)    = Right . (normalizerFor options) $ ast

parserFor :: Language -> EitherParser
parserFor Haskell        = parseHaskell
parserFor Java           = parseJava
parserFor JavaScript     = maybeToEither parseJavaScript
parserFor Prolog         = parseProlog
parserFor Python         = parsePython

normalizerFor :: (Maybe NormalizationOptions) -> (Expression -> Expression)
normalizerFor Nothing        = normalize
normalizerFor (Just options) = normalizeWith options
