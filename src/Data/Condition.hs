module Data.Condition (orElse, andAlso, never) where

type Condition a = a -> Bool

andAlso :: Condition a -> Condition a -> Condition a
andAlso f1 f2 a = f1 a && f2 a

orElse :: Condition a -> Condition a -> Condition a
orElse f1 f2 a = f1 a || f2 a

never :: Condition a -> Condition a
never f = not . f

