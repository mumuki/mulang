module Data.List.Extra (
  headOrElse,
  has,
  dropLast,
  unwind) where

headOrElse :: a -> [a] -> a
headOrElse x []     = x
headOrElse _ (x:_)  = x

has :: (a -> Bool) -> (b -> [a]) -> (b -> Bool)
has f g = any f . g

dropLast :: Int -> String -> String
dropLast n xs = take (length xs - n) xs

unwind :: [a] -> Maybe ([a], a)
unwind [] = Nothing
unwind xs = Just (init xs, last xs)
