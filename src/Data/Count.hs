module Data.Count (
  encodeMaybe,
  encode,
  cast,
  decode,
  count,
  counts,
  Count (..)) where

data Count
  = Zero
  | One
  | MoreThanOne Int
  deriving (Show, Eq, Ord)

encodeMaybe :: Integral a => a -> Maybe Count
encodeMaybe 0 = Just Zero
encodeMaybe 1 = Just One
encodeMaybe n | n >= 1 = Just $ MoreThanOne (fromIntegral n)
encodeMaybe _ = Nothing

encode :: Integral a => a -> Count
encode n | Just m <- encodeMaybe n = m

cast :: Integral a => a -> Count
cast n | Just m <- encodeMaybe n = m
cast _ = Zero

decode :: Integral a => Count -> a
decode Zero = 0
decode One = 1
decode (MoreThanOne n) = fromIntegral n

count :: [a] -> Count
count []     = Zero
count [_]    = One
count xs     = MoreThanOne (length xs)

counts :: (a -> Bool) -> (b -> [a]) -> b -> Count
counts f g = count . filter f . g

instance  Num Count  where
  negate _ = Zero

  abs x = x
  signum x = x

  Zero + n = n
  n    + Zero = n
  One  + One    = MoreThanOne 2
  One  + (MoreThanOne n) = MoreThanOne (n+1)
  (MoreThanOne n) + One  = MoreThanOne (n+1)
  (MoreThanOne n) + (MoreThanOne m) = MoreThanOne (n+m)

  fromInteger = cast

  n - Zero                      = n
  One - _                       = Zero
  (MoreThanOne n) - One           = cast n
  (MoreThanOne n) - (MoreThanOne m) = cast (n-m)

  Zero * _ = Zero
  _    * Zero = Zero
  One  * n    = n
  n    * One = n
  (MoreThanOne n) * (MoreThanOne m) = MoreThanOne (n*m)


