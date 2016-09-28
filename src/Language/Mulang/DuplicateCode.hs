module Language.Mulang.DuplicateCode (hasDuplicateCode) where


import  Data.HashMap.Lazy as  HashMap (HashMap, lookup, member,insert,empty,fromList)
import  Language.Mulang
import  Language.Mulang.Inspector
import  Language.Mulang.Parsers.Gobstones

hasDuplicateCode :: Expression -> Bool
hasDuplicateCode (Sequence xs) = hasDuplicateInList xs []
hasDuplicateCode x = hasDuplicateInList [x] []

--Temporalmente solo para ser usado en Gobstones
hasDuplicateInList :: [Expression] -> [[Equation]] -> Bool
hasDuplicateInList [] bodyL                                              = False
hasDuplicateInList ((ProcedureDeclaration _ eq):xs) bodyL                = (elem eq bodyL) || hasDuplicateInList xs (eq : bodyL)
hasDuplicateInList ((FunctionDeclaration _ eq):xs) bodyL                 = (elem (filterReturn eq) bodyL) || hasDuplicateInList xs (filterReturn eq : bodyL)
hasDuplicateInList (x:xs) bodyL                                          = hasDuplicateInList xs bodyL

filterReturn [] = []
filterReturn ((Equation patterns eqB):xs) =  Equation patterns (filterReturnInEqBody eqB) : filterReturn xs 
 
filterReturnInEqBody (UnguardedBody (Sequence xs)) = UnguardedBody $ simplify (Sequence $ init xs) -- el return es el ultimo elemento en Gobstones
filterReturnInEqBody (UnguardedBody (Return e)) = UnguardedBody MuNull


