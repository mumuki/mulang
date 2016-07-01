module Control.Fallible where

class Fallible f where
  failure :: f v -> Either v String

instance Show e => Fallible (Either e) where
  failure (Left e)  = Right . show $ e
  failure (Right v) = (Left v)


orFail :: Fallible f => f v -> v
orFail f = case failure f of
            (Left v)  -> v
            (Right e) -> error e

orNothing :: Fallible f => f v -> Maybe v
orNothing f = case failure f of
            (Left v) -> Just v
            _        -> Nothing




