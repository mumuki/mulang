module Language.Mulang.Analyzer.DomainLanguageAnalyzer (
  analyseDomainLanguage) where

import qualified Language.Mulang.DomainLanguage as DL

import Language.Mulang.Ast (Expression)
import Language.Mulang.Inspector.Combiner (detect)
import Language.Mulang.Unfold (allExpressions, mainExpressions)
import Language.Mulang.Analyzer.Analysis

import Text.Inflections.Tokenizer (camelCase, snakeCase)
import Text.Dictionary (fromFile, toDictionary)


analyseDomainLanguage :: Expression -> Maybe DomainLanguage -> DomainLanguageViolations
analyseDomainLanguage code Nothing   = noDomainLanguageViolations
analyseDomainLanguage code (Just language)
  =  DomainLanguageViolations {
        tooShortBindings = detectWithLanguage DL.hasTooShortBindings,
        misspelledBindings = []
      }

    where
      detectWithLanguage inspection = detect (DL.unfold compiledLang) (inspection compiledLang) code
      compiledLang = compileLanguage language

compileLanguage (DomainLanguage _ style strict size)
  = DL.DomainLanguage (toDictionary []) (compileStyle style) (compileUnfold strict) (compileSize size)

   where
    compileSize (Just n) = n
    compileSize _        = 3

    compileUnfold (Just True) = allExpressions
    compileUnfold _           = mainExpressions

    compileStyle (Just SnakeCase) = snakeCase
    compileStyle _                = camelCase

