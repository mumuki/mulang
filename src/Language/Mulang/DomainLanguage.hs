module Language.Mulang.DomainLanguage (
  hasTooShortIdentifiers,
  hasWrongCaseIdentifiers,
  hasMisspelledIdentifiers,
  DomainLanguage(..)) where

import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Generator (mainDeclaredIdentifiers)

import Text.Dictionary (Dictionary, exists)

import Text.Inflections.Tokenizer (CaseStyle, tokenize, canTokenize)


data DomainLanguage = DomainLanguage {
                          dictionary :: Dictionary,
                          caseStyle :: CaseStyle,
                          minimumIdentifierSize :: Int,
                          jargon :: [String] }

type DomainLanguageInspection = DomainLanguage -> Inspection

hasTooShortIdentifiers :: DomainLanguageInspection
hasTooShortIdentifiers language = any isShort . mainDeclaredIdentifiers
  where isShort identifier = length identifier < (minimumIdentifierSize language) && notJargonOf identifier language

hasMisspelledIdentifiers :: DomainLanguageInspection
hasMisspelledIdentifiers language | emptyDictionary language = const False
hasMisspelledIdentifiers language = any isMisspelled  . wordsOf language
  where isMisspelled identifier = not (identifier `exists` dictionary language) && notJargonOf identifier language

hasWrongCaseIdentifiers :: DomainLanguageInspection
hasWrongCaseIdentifiers (DomainLanguage _ style _ _)
  = any (not . canTokenize style) . mainDeclaredIdentifiers

wordsOf :: DomainLanguage -> Expression -> [String]
wordsOf (DomainLanguage _ style _ _) = concatMap (tokenize style) . mainDeclaredIdentifiers

emptyDictionary = null . dictionary

notJargonOf identifier language = notElem identifier (jargon language)
