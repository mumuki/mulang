module Language.Mulang.Analyzer.DomainLanguageAnalyzer (
  analyseDomainLanguage) where

import qualified Language.Mulang.DomainLanguage as DL

import Language.Mulang.Ast (Expression)
import Language.Mulang.Inspector.Combiner (detect)
import Language.Mulang.Unfold (mainExpressions)
import Language.Mulang.Analyzer.Analysis

import Text.Inflections.Tokenizer (camelCase, snakeCase)
import Text.Dictionary (fromFile, toDictionary)


analyseDomainLanguage :: Expression -> Maybe DomainLanguage -> DomainLanguageViolations
analyseDomainLanguage code Nothing   = noDomainLanguageViolations
analyseDomainLanguage code (Just language)
  =  DomainLanguageViolations {
        tooShortBindings = detectWithLanguage DL.hasTooShortBindings,
        wrongCaseBindings = detectWithLanguage DL.hasWrongCaseBindings,
        misspelledBindings = []
      }

    where
      detectWithLanguage inspection = detect mainExpressions (inspection compiledLang) code
      compiledLang = compileLanguage language

compileLanguage (DomainLanguage _ style size)
  = DL.DomainLanguage (toDictionary []) (compileStyle style) (compileSize size)

   where
    compileSize (Just n) = n
    compileSize _        = 3

    compileStyle (Just SnakeCase) = snakeCase
    compileStyle _                = camelCase

