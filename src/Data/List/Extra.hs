module Data.List.Extra (
  headOrElse,
  has,
  dropLast) where

headOrElse :: a -> [a] -> a
headOrElse x []     = x
headOrElse _ (x:_)  = x

has :: (a -> Bool) -> (b -> [a]) -> (b -> Bool)
has f g = any f . g

dropLast :: Int -> String -> String
dropLast n xs = take (length xs - n) xs
