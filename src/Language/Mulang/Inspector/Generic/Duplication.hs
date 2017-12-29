module Language.Mulang.Inspector.Generic.Duplication (hasCodeDuplication) where

import           Language.Mulang.Ast
import           Language.Mulang.Inspector
import           Language.Mulang.Generator (expressions)
import qualified Data.Hashable as H (hash)
import           Data.List (nub, subsequences)

hasCodeDuplication :: Inspection
hasCodeDuplication e =  hasDuplicates $ map hash $ filter (not . isLightweight) $ concat $ stripesOf 2 e

isLightweight :: Inspection
isLightweight (MuNumber _)              = True
isLightweight (MuString _)              = True
isLightweight (MuBool _)                = True
isLightweight (Reference _)             = True
isLightweight Self                      = True
isLightweight MuNull                    = True
isLightweight MuNil                    = True
isLightweight Equal                     = True
isLightweight (Application _ es)        = not $ any isApplication es
isLightweight (Return e)                = isLightweight e
isLightweight (Assignment _ e)  = isLightweight e
isLightweight (Variable _ e) = isLightweight e
isLightweight _                         = False

isApplication (Application _ _) = True
isApplication _                 = False


hasDuplicates ::Eq a => [a] -> Bool
hasDuplicates xs = nub xs /= xs


hash :: Expression -> Int
hash (Return e)                    = 1 * (37 + hash e)
hash (MuNumber e)                  = 2 * H.hash e
hash (MuString e)                  = 3 * H.hash e
hash (Reference i)                 = 5 * H.hash i
hash (MuBool e)                    = 7 * H.hash e
hash (Application i es)            = 11 * (37 + hash i) * (positionalHash es)
hash (SimpleFunction _ _ body)     = 13 * (37 + hash body)
hash Self                          = 15
hash (SimpleProcedure _ _ body)    = 17 * (37 + hash body)
hash (Sequence es)                 = 19 * (37 + positionalHash es)
hash _                             = 1

positionalHash :: [Expression] -> Int
positionalHash = sum . zipWith hashElement [1..] . reverse
        where
          hashElement :: Int -> Expression -> Int
          hashElement index expression = (31^index) * hash expression

stripesOf :: Int -> Expression -> [[Expression]]
stripesOf n = concatMap (makeStripes n) . expressions

makeStripes :: Int -> Expression -> [[Expression]]
makeStripes n (Sequence xs) = stripes n xs
makeStripes _ e             = [[e]]

stripes :: Int -> [a] -> [[a]]
stripes n = filter ( (>n) . length) . subsequences

