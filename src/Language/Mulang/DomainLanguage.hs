module Language.Mulang.DomainLanguage (
  hasTooShortBindings,
  hasWrongCaseBindings,
  hasMisspelledBindings,
  DomainLanguage(..)) where

import Language.Mulang.Unfold (Unfold)
import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Explorer (declaredBindingsOf)

import Text.Dictionary (Dictionary, exists)

import Text.Inflections.Tokenizer (CaseStyle, tokenize, canTokenize)

data DomainLanguage = DomainLanguage {
                          dictionary :: Dictionary,
                          caseStyle :: CaseStyle,
                          unfold :: Unfold,
                          minimumBindingSize :: Int }

hasTooShortBindings :: DomainLanguage -> Inspection
hasTooShortBindings (DomainLanguage _ _ unfold size)
  = any ((<size).length) . declaredBindingsOf unfold

hasMisspelledBindings :: DomainLanguage -> Inspection
hasMisspelledBindings language
  = any (not . (`exists` (dictionary language)))  . wordsOf language

hasWrongCaseBindings :: DomainLanguage -> Inspection
hasWrongCaseBindings (DomainLanguage _ style unfold _)
  = any (not . canTokenize style) . declaredBindingsOf unfold

wordsOf :: DomainLanguage -> Expression -> [String]
wordsOf (DomainLanguage _ style unfold _) = concatMap (tokenize style) . declaredBindingsOf unfold


