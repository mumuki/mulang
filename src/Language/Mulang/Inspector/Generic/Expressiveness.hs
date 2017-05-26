module Language.Mulang.Inspector.Generic.Expressiveness (
  wordsOf,
  isWellWritten,
  english,
  Dictionary) where

import Language.Mulang.Inspector
import Language.Mulang.Explorer (declaredBindingsOf)

import Text.Inflections.Tokenizer

type Dictionary = [String]

english :: Dictionary
english = ["today", "is", "a", "great", "day"]

isWellWritten :: Dictionary -> Inspection
isWellWritten dictionary = all (`elem` dictionary)  . wordsOf

wordsOf = concatMap (tokenize camelCase) . declaredBindingsOf