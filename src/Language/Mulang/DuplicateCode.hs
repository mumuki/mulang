module Language.Mulang.DuplicateCode (hasDuplicateCode) where


import Language.Mulang
import Language.Mulang.Explorer (expressionsOf)
import qualified Data.Hashable as H (hash)
import Data.List (nub)




hasDuplicateCode :: Expression -> Bool
hasDuplicateCode (Sequence xs) =  hasDuplicates (map hash (filter (not . isLightweight) (concatMap expressionsOf xs)))

isLightweight :: Expression -> Bool
isLightweight (MuNumber e)              = True
isLightweight (MuString e)              = True
isLightweight (MuBool e)                = True
isLightweight (Variable i)              = True
isLightweight (Application i es)        = not $ any isApplication es
--isLightweight (Application i es)        = all isLightweight es
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