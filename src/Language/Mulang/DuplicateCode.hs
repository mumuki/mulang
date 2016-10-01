module Language.Mulang.DuplicateCode (hasDuplicateCode) where


import  Data.HashMap.Lazy as  HashMap (HashMap, lookup, member,insert,empty,fromList)
import  Language.Mulang
import  Language.Mulang.Inspector
import  Language.Mulang.Parsers.Gobstones
import  Data.List

hasDuplicateCode :: Expression -> Bool
hasDuplicateCode (Sequence xs) = hasDuplicateInList xs [] []
hasDuplicateCode x             = hasDuplicateInList [x] [] []

--Temporalmente solo para ser usado en Gobstones
--Falta generalizar los nombres de los parametros y las varaibles usadas en la busqueda de codigo repetido
hasDuplicateInList :: [Expression] -> [[Equation]] -> [Expression] -> Bool
hasDuplicateInList [] bodyL expL                                             = False
hasDuplicateInList ((ProcedureDeclaration _ eq):xs) bodyL expL               = let exps = (getExpressionsInEquation eq) in 
                                                                                          (elem eq bodyL) || (not (null (intersect exps expL))) || hasDuplicateInList xs (addIfNotEmpty (removeExpressionsInEquations removeSentencesInEqBody eq) bodyL) (exps ++ expL)
hasDuplicateInList ((FunctionDeclaration _ eq):xs) bodyL expL                = let exps = (getExpressionsInEquation eq) in
                                                                                          (elem (removeExpressionsInEquations removeReturnInEqBody eq) bodyL) || (not (null (intersect exps expL))) || hasDuplicateInList xs (addIfNotEmpty (removeExpressionsInEquations (removeSentencesInEqBody . removeReturnInEqBody) eq) bodyL) (exps ++ expL)
hasDuplicateInList (x:xs) bodyL expL                                         = hasDuplicateInList xs bodyL expL

addIfNotEmpty :: [Equation] -> [[Equation]] -> [[Equation]]
addIfNotEmpty xs ys | [] <- xs  = ys
                    | otherwise = xs : ys

removeExpressionsInEquations :: (EquationBody -> EquationBody) -> [Equation] -> [Equation]
removeExpressionsInEquations f []                           = []
removeExpressionsInEquations f ((Equation patterns eqB):xs) | UnguardedBody MuNull <- f eqB = removeExpressionsInEquations f xs
                                                 | otherwise = (Equation patterns (f eqB)) : removeExpressionsInEquations f xs
                                      
removeSentencesInEqBody :: EquationBody -> EquationBody
removeSentencesInEqBody (UnguardedBody (Sequence xs))                                                  = UnguardedBody $ simplify (Sequence $ filter notIsWorstSentence xs)
removeSentencesInEqBody (UnguardedBody e)  | [] <- filter notIsWorstSentence [e]                       = UnguardedBody MuNull
                                           | otherwise                                                 = UnguardedBody e

removeReturnInEqBody :: EquationBody -> EquationBody
removeReturnInEqBody (UnguardedBody (Sequence xs)) = UnguardedBody $ simplify (Sequence $ init xs) -- el return es el ultimo elemento en Gobstones
removeReturnInEqBody (UnguardedBody (Return e))    = UnguardedBody MuNull
removeReturnInEqBody x                             = x

getExpressionsInEquation :: [Equation] -> [Expression]
getExpressionsInEquation []                           = []
getExpressionsInEquation ((Equation patterns eqB):xs) = filter notIsLiteral $ getExpressionsEqBody eqB ++ getExpressionsInEquation xs

notIsLiteral :: Expression -> Bool
notIsLiteral (MuSymbol _)                      = False
notIsLiteral (MuNumber _)                      = False
notIsLiteral (MuBool _)                        = False
notIsLiteral (Variable _)                      = False
notIsLiteral (Application (Variable "+") exps) = False
notIsLiteral _                                 = True

notIsWorstSentence :: Expression -> Bool
notIsWorstSentence (VariableAssignment var (Application (Variable "+") exps))   = False
notIsWorstSentence (VariableDeclaration var (Application (Variable "+") exps))  = False
notIsWorstSentence _                                                            = True

getExpressionsEqBody :: EquationBody -> [Expression]
getExpressionsEqBody (UnguardedBody (Sequence xs)) = getExpressions xs  
getExpressionsEqBody (UnguardedBody e)             = getExpressions [e]

getExpressions :: [Expression] -> [Expression]  
getExpressions []                                 = []
getExpressions ((Return  e):xs)                   = e : getExpressions xs
getExpressions ((If  e b1 b2):xs)                 = e : getExpressions xs ++ getExpressions [b1] ++ getExpressions [b2]
getExpressions ((While  e b):xs)                  = e : getExpressions xs ++ getExpressions [b]
getExpressions ((Repeat  e b):xs)                 = e : getExpressions xs ++ getExpressions [b]
getExpressions ((VariableDeclaration i e):xs)     = e : getExpressions xs
getExpressions ((VariableAssignment i e):xs)      = e : getExpressions xs
getExpressions ((Switch e cases):xs)              = e : getExpressions xs
getExpressions ((Application e exps):xs)          = exps ++ getExpressions xs
getExpressions (x:xs)                             = getExpressions xs

