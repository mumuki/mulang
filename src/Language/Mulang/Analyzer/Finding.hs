module Language.Mulang.Analyzer.Finding (
  mock,
  mockInspection,
  mapFindings,
  Finding) where

import Language.Mulang.Inspector.Primitive (Inspection, lenient)

type Finding e = Either String e

mock :: e -> Finding e -> e
mock replacement = either (const replacement) id

mockInspection :: Finding Inspection -> Inspection
mockInspection = mock lenient

mapFindings :: (a -> Finding b) -> [a] -> Finding [b]
mapFindings f = foldr accum (return []) . map f
  where
    accum (Right x) (Right xs) = Right (x:xs)
    accum (Left m)  (Right _)  = Left m
    accum (Left m2) (Left m1)  = Left (m1 ++ ";" ++ m2)
    accum  _        (Left m)  = Left m
