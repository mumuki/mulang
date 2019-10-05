module Language.Mulang.Analyzer.Finding (
  mock,
  mapFindings,
  Finding) where

type Finding e = Either String e

mock :: e -> Finding e -> Finding e
mock replacement = Right . either (const replacement) (id)

mapFindings :: (a -> Finding b) -> [a] -> Finding [b]
mapFindings f = foldr accum (return []) . map f
  where
    accum (Right x) (Right xs) = Right (x:xs)
    accum (Left m)  (Right _)  = Left m
    accum (Left m2) (Left m1)  = Left (m1 ++ ";" ++ m2)
    accum  _        (Left m)   = Left m
