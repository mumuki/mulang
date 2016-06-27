module Language.Mulang.Inspector.Expressiveness (
  wordsOf,
  isWellWritten,
  english,
  Dictionary) where

import Language.Mulang.Binding
import Language.Mulang.Inspector
import Language.Mulang.Explorer (declaredBindingsOf)

import Text.Inflections
import Text.Inflections.Parse.Types

import Data.Char

type Dictionary = [String]

english :: Dictionary
english = ["today", "is", "a", "great", "day"]


isWellWritten :: Dictionary -> Inspection
isWellWritten dictionary = all (`elem` dictionary)  . wordsOf

wordsOf = concatMap tokenize . declaredBindingsOf

tokenize :: String -> [String]
tokenize s | Right words <- parseCamelCase [] s = concatMap toToken words
           | otherwise = []
           where
              toToken (Word w) = [map toLower w]
              toToken _        = []







