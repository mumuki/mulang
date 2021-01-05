module Data.Function.Extra (compose2, orElse, andAlso, never, (<==)) where

import Data.Function (on)

andAlso :: (a -> Bool) -> (a -> Bool) -> a -> Bool
andAlso = compose2 (&&)

orElse :: (a -> Bool) -> (a -> Bool) -> a -> Bool
orElse = compose2 (||)

never :: (a -> Bool) -> a -> Bool
never f = not . f

compose2 :: (a -> a -> b) -> (c -> a) -> (c -> a) -> c -> b
compose2 op f1 f2 a = on op ($ a) f1 f2

(<==) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(<==) = ((.).(.))

