module Language.Mulang.Analyzer.DomainLanguageCompiler (
  emptyDomainLanguage,
  compileDomainLanguage) where

import           Data.Maybe (fromMaybe)
import           Language.Mulang.Analyzer.Analysis
import qualified Language.Mulang.DomainLanguage as DL
import           Text.Inflections.Tokenizer (camelCase, snakeCase)
import           Text.Dictionary (fromFile, toDictionary)


emptyDomainLanguage :: DomainLanguage
emptyDomainLanguage = DomainLanguage Nothing Nothing Nothing Nothing

compileDomainLanguage :: Maybe DomainLanguage -> IO DL.DomainLanguage
compileDomainLanguage Nothing                                 = compileDomainLanguage (Just emptyDomainLanguage)
compileDomainLanguage (Just (DomainLanguage path style size jargon)) = do
  dictionary <- compileDictionay path
  return $ DL.DomainLanguage dictionary (compileStyle style) (compileSize size) (compileJargon jargon)

  where
    compileDictionay (Just path) = fromFile path
    compileDictionay _           = return $ toDictionary []

    compileSize = fromMaybe 3

    compileStyle (Just SnakeCase) = snakeCase
    compileStyle _                = camelCase

    compileJargon = fromMaybe []
