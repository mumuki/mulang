module Data.List.Extra (headOrElse) where

headOrElse:: a -> [a] -> a
headOrElse x []     = x
headOrElse _ (x:_)  = x
