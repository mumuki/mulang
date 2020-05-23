{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Autocorrector.Ruby (rubyAutocorrectionRules) where

rubyAutocorrectionRules :: [(String, String)]
rubyAutocorrectionRules = [
    ("if", "UsesIf"),
    ("return", "Returns"),
    ("class", "DeclaresClass"),
    ("def", "DeclaresComputation"),
    ("for", "UsesForeach"),
    ("include",  "Includes")
  ]
