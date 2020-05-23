{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Autocorrector.Haskell (haskellAutocorrectionRules) where

haskellAutocorrectionRules :: [(String, String)]
haskellAutocorrectionRules = [
    ("type", "DeclaresTypeAlias"),
    ("if", "UsesIf")
  ]

