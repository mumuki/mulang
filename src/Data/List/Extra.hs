module Data.List.Extra (
  headOrElse,
  has) where

headOrElse :: a -> [a] -> a
headOrElse x []     = x
headOrElse _ (x:_)  = x

has :: (a -> Bool) -> (b -> [a]) -> (b -> Bool)
has f g = any f . g
