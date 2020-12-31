module        Language.Mulang.Analyzer.FragmentParser (
  parseFragment',
  parseFragment) where

import        Control.Fallible (orFail)

import        Language.Mulang
import        Language.Mulang.Parsers (EitherParser, maybeToEither)
import        Language.Mulang.Parsers.Haskell
import        Language.Mulang.Parsers.C (parseC)
import        Language.Mulang.Parsers.JavaScript (parseJavaScript)
import        Language.Mulang.Parsers.Prolog (parseProlog)
import        Language.Mulang.Parsers.Java (parseJava)
import        Language.Mulang.Parsers.Python (parsePython, parsePython2, parsePython3)
import        Language.Mulang.Analyzer.Analysis (Fragment(..), Language(..))
import        Language.Mulang.Transform.Normalizer (normalize, NormalizationOptions)

parseFragment' :: Maybe NormalizationOptions -> Fragment -> Expression
parseFragment' options = orFail . parseFragment options

parseFragment :: Maybe NormalizationOptions -> Fragment -> Either String Expression
parseFragment options = fmap (normalizerFor options) . parse
  where
    parse (CodeSample language content) = (parserFor language) content
    parse (MulangSample ast)            = Right ast

parserFor :: Language -> EitherParser
parserFor C              = parseC
parserFor Haskell        = parseHaskell
parserFor Java           = parseJava
parserFor JavaScript     = maybeToEither parseJavaScript
parserFor Prolog         = parseProlog
parserFor Python         = parsePython
parserFor Python2        = parsePython2
parserFor Python3        = parsePython3

normalizerFor :: Maybe NormalizationOptions -> (Expression -> Expression)
normalizerFor Nothing        = id
normalizerFor (Just options) = normalize options
