module Language.Mulang.Inspector.Generic.Expressiveness (
  wordsOf,
  isMisspelled) where

import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Explorer (declaredBindingsOf)

import Text.Dictionary (Dictionary, exists)

import Text.Inflections.Tokenizer (CaseStyle, tokenize)

isMisspelled :: CaseStyle -> Dictionary -> Inspection
isMisspelled style dictionary = any (not . (`exists` dictionary))  . wordsOf style

wordsOf :: CaseStyle -> Expression -> [String]
wordsOf style = concatMap (tokenize style) . declaredBindingsOf


