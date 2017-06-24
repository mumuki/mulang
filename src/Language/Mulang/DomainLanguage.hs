module Language.Mulang.DomainLanguage (
  hasTooShortBindings,
  hasWrongCaseBindings,
  hasMisspelledBindings,
  DomainLanguage(..)) where

import Language.Mulang.Unfold (mainExpressions)
import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Explorer (declaredBindingsOf)

import Text.Dictionary (Dictionary, exists)

import Text.Inflections.Tokenizer (CaseStyle, tokenize, canTokenize)


data DomainLanguage = DomainLanguage {
                          dictionary :: Dictionary,
                          caseStyle :: CaseStyle,
                          minimumBindingSize :: Int }

type DomainLanguageInspection = DomainLanguage -> Inspection

hasTooShortBindings :: DomainLanguageInspection
hasTooShortBindings (DomainLanguage _ _ size)
  = any ((<size).length) . mainDeclaredBindingsOf

hasMisspelledBindings :: DomainLanguageInspection
hasMisspelledBindings language
  = any (not . (`exists` (dictionary language)))  . wordsOf language

hasWrongCaseBindings :: DomainLanguageInspection
hasWrongCaseBindings (DomainLanguage _ style _)
  = any (not . canTokenize style) . mainDeclaredBindingsOf

wordsOf :: DomainLanguage -> Expression -> [String]
wordsOf (DomainLanguage _ style _) = concatMap (tokenize style) . mainDeclaredBindingsOf


mainDeclaredBindingsOf = declaredBindingsOf mainExpressions
