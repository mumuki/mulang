{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Autocorrector.Python (pythonAutocorrectionRules) where

pythonAutocorrectionRules :: [(String, String)]
pythonAutocorrectionRules = [
    ("if", "UsesIf"),
    ("return", "Returns"),
    ("class", "DeclaresClass"),
    ("def", "DeclaresComputation"),
    ("for", "UsesForeach")
  ]
