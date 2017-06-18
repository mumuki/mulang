module Language.Mulang.Inspector.Generic.Expressiveness (
  wordsOf,
  isWellWritten) where

import Language.Mulang.Inspector (Inspection)
import Language.Mulang.Ast (Expression)
import Language.Mulang.Explorer (declaredBindingsOf)

import Text.Dictionary (Dictionary, exists)

import Text.Inflections.Tokenizer (CaseStyle, tokenize)

isWellWritten :: CaseStyle -> Dictionary -> Inspection
isWellWritten style dictionary = all (`exists` dictionary)  . wordsOf style

wordsOf :: CaseStyle -> Expression -> [String]
wordsOf style = concatMap (tokenize style) . declaredBindingsOf


