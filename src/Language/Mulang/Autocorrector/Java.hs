{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Autocorrector.Java (javaAutocorrectionRules) where

javaAutocorrectionRules :: [(String, String)]
javaAutocorrectionRules = [
    ("if", "UsesIf"),
    ("return", "Returns"),
    ("class", "DeclaresClass"),
    ("interface", "DeclaresInterface"),
    ("for", "UsesForLoop")
  ]
