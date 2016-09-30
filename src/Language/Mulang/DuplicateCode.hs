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
                                                                                          (elem eq bodyL) || (not (null (intersect exps expL))) || hasDuplicateInList xs (eq : bodyL) (exps ++ expL)
hasDuplicateInList ((FunctionDeclaration _ eq):xs) bodyL expL                = let exps = (getExpressionsInEquation eq) in
                                                                                          (elem (filterReturn eq) bodyL) || (not (null (intersect exps expL))) || hasDuplicateInList xs (addEqsInBodyL (filterReturn eq) bodyL) (exps ++ expL)
hasDuplicateInList (x:xs) bodyL expL                                         = hasDuplicateInList xs bodyL expL

addEqsInBodyL :: [Equation] -> [[Equation]] -> [[Equation]]
addEqsInBodyL [] ys = ys
addEqsInBodyL l@(x:xs) ys = l : ys

filterReturn :: [Equation] -> [Equation]
filterReturn []                           = []
filterReturn ((Equation patterns eqB):xs) | UnguardedBody MuNull <- (filterReturnInEqBody eqB) = filterReturn xs
                                          | otherwise = (Equation patterns (filterReturnInEqBody eqB)) : filterReturn xs
 
filterReturnInEqBody :: EquationBody -> EquationBody
filterReturnInEqBody (UnguardedBody (Sequence xs)) = UnguardedBody $ simplify (Sequence $ init xs) -- el return es el ultimo elemento en Gobstones
filterReturnInEqBody (UnguardedBody (Return e))    = UnguardedBody MuNull


getExpressionsInEquation :: [Equation] -> [Expression]
getExpressionsInEquation []                           = []
getExpressionsInEquation ((Equation patterns eqB):xs) = getExpressionsEqBody eqB ++ getExpressionsInEquation xs

getExpressionsEqBody :: EquationBody -> [Expression]
getExpressionsEqBody (UnguardedBody (Sequence xs)) = takeLiterals $ getExpressions xs  
getExpressionsEqBody (UnguardedBody e)             = takeLiterals $ getExpressions [e]

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

takeLiterals :: [Expression] -> [Expression]
takeLiterals []                  = []
takeLiterals ((MuSymbol _):xs)   = takeLiterals xs
takeLiterals ((MuNumber _):xs)   = takeLiterals xs
takeLiterals ((MuBool _):xs)     = takeLiterals xs
takeLiterals ((Variable _):xs)   = takeLiterals xs
takeLiterals (x:xs)              = x : takeLiterals xs