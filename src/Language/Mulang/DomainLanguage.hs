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
                          minimumBindingSize :: Int,
                          jargon :: [String] }

type DomainLanguageInspection = DomainLanguage -> Inspection

hasTooShortBindings :: DomainLanguageInspection
hasTooShortBindings language = any isShort . mainDeclaredBindingsOf
  where isShort binding = length binding < (minimumBindingSize language) && notJargonOf binding language

hasMisspelledBindings :: DomainLanguageInspection
hasMisspelledBindings language | emptyDictionary language = const False
hasMisspelledBindings language = any isMisspelled  . wordsOf language
  where isMisspelled binding = not (binding `exists` dictionary language) && notJargonOf binding language

hasWrongCaseBindings :: DomainLanguageInspection
hasWrongCaseBindings (DomainLanguage _ style _ _)
  = any (not . canTokenize style) . mainDeclaredBindingsOf

wordsOf :: DomainLanguage -> Expression -> [String]
wordsOf (DomainLanguage _ style _ _) = concatMap (tokenize style) . mainDeclaredBindingsOf


mainDeclaredBindingsOf = declaredBindingsOf mainExpressions

emptyDictionary = null . dictionary

notJargonOf binding language = notElem binding (jargon language)
