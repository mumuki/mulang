module Language.Mulang.Inspector.CodeDuplication (hasCodeDuplication,f) where


import Language.Mulang
import Language.Mulang.Explorer (expressionsOf)
import qualified Data.Hashable as H (hash)
import Data.List (nub, subsequences)




hasCodeDuplication :: Expression -> Bool
hasCodeDuplication e =  hasDuplicates (map hash (filter (not . isLightweight) (concat $ stripesOf 2 e)))

f e = map hash (filter (not . isLightweight) (concat $ stripesOf 0 e))

isLightweight :: Expression -> Bool
isLightweight (MuNumber e)              = True
isLightweight (MuString e)              = True
isLightweight (MuBool e)                = True
isLightweight (Variable i)              = True
isLightweight MuNull                    = True
isLightweight Equal                     = True
isLightweight (Application i es)        = not $ any isApplication es
isLightweight (Return e)                = isLightweight e
isLightweight (VariableAssignment i e)  = isLightweight e
isLightweight (VariableDeclaration i e) = isLightweight e
isLightweight _                         = False

isApplication (Application i es) = True
isApplication _                  = False


hasDuplicates ::Eq a => [a] -> Bool
hasDuplicates xs = nub xs /= xs


hash :: Expression -> Int
hash (Return e)                    = 1 * (37 + hash e)
hash (MuNumber e)                  = 2 * H.hash e
hash (MuString e)                  = 3 * H.hash e
hash (Variable i)                  = 5 * H.hash i
hash (MuBool e)                    = 7 * H.hash e
hash (Application i es)            = 11 * (37 + hash i) * (positionalHash es)
hash f@(FunctionDeclaration _ _)   = 13 * (37 + hash (simpleFunctionBody f))
hash f@(ProcedureDeclaration _ _)  = 17 * (37 + hash (simpleProcedureBody f))
hash (Sequence es)                 = 19 * (37 + positionalHash es)
hash _                             = 1

simpleProcedureBody :: Expression -> Expression
simpleProcedureBody (ProcedureDeclaration _ [equation]) = equationUnguardedBody equation

simpleFunctionBody :: Expression -> Expression
simpleFunctionBody (FunctionDeclaration _ [equation]) = equationUnguardedBody equation
equationUnguardedBody (Equation _ (UnguardedBody body)) = body


positionalHash :: [Expression] -> Int
positionalHash = sum . zipWith (\index expression -> (31^index) * hash expression) [1..] . reverse


stripesOf :: Int -> Expression -> [[Expression]]
stripesOf n = concatMap (makeStripes n) . expressionsOf

makeStripes :: Int -> Expression -> [[Expression]]
makeStripes n (Sequence xs) = stripes n xs
makeStripes _ e             = [[e]]

stripes :: Int -> [a] -> [[a]]
stripes n = filter ( (>n) . length) . subsequences

