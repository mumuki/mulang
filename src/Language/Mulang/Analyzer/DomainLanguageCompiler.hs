module Language.Mulang.Analyzer.DomainLanguageCompiler (
  emptyDomainLanguage,
  compileDomainLanguage) where

import           Language.Mulang.Analyzer.Analysis
import qualified Language.Mulang.DomainLanguage as DL
import           Text.Inflections.Tokenizer (camelCase, snakeCase)
import           Text.Dictionary (fromFile, toDictionary)


emptyDomainLanguage :: DomainLanguage
emptyDomainLanguage = DomainLanguage Nothing Nothing Nothing

compileDomainLanguage :: Maybe DomainLanguage -> IO DL.DomainLanguage
compileDomainLanguage Nothing                                 = compileDomainLanguage (Just emptyDomainLanguage)
compileDomainLanguage (Just (DomainLanguage path style size)) = do
  dictionary <- compileDictionay path
  return $ DL.DomainLanguage dictionary (compileStyle style) (compileSize size)

  where
    compileDictionay (Just path) = fromFile path
    compileDictionay _           = return $ toDictionary []

    compileSize (Just n) = n
    compileSize _        = 3

    compileStyle (Just SnakeCase) = snakeCase
    compileStyle _                = camelCase


