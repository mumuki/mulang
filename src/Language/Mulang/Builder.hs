{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Builder (
    merge,
    compact,
    compactMap,
    compactConcatMap,
    normalize,
    normalizeWith,
    defaultNormalizationOptions,
    NormalizationOptions (..),
    SequenceSortMode (..)) where

import           GHC.Generics

import Data.List (sort, nub)
import Data.List.Extra (unwind)

import Language.Mulang.Ast
import Language.Mulang.Generator (declarators, declaredIdentifiers)
import Language.Mulang.Inspector.Literal (isLiteral)

data NormalizationOptions = NormalizationOptions {
  convertObjectVariableIntoObject :: Bool,
  convertLambdaVariableIntoFunction :: Bool,
  convertObjectLevelFunctionIntoMethod :: Bool,
  convertObjectLevelLambdaVariableIntoMethod :: Bool,
  convertObjectLevelVariableIntoAttribute :: Bool,
  sortSequenceDeclarations :: SequenceSortMode,
  insertImplicitReturn :: Bool
} deriving (Eq, Show, Read, Generic)

data SequenceSortMode
  = SortNothing
  | SortUniqueNonVariables
  | SortAllNonVarables
  | SortAll deriving (Eq, Show, Read, Generic)

compactConcatMap :: (a -> [Expression]) -> [a] -> Expression
compactConcatMap f = compact . concat . map f

compactMap :: (a -> Expression) -> [a] -> Expression
compactMap f = compact . map f

compact :: [Expression] -> Expression
compact []  = None
compact [e] = e
compact es  = Sequence es

merge :: Expression -> Expression -> Expression
merge e1 None                      = e1
merge None e2                      = e2
merge (Sequence s1) (Sequence s2)  = Sequence (s1 ++ s2)
merge (Sequence s1) e2             = Sequence (s1 ++ [e2])
merge e1            (Sequence s2)  = Sequence (e1 : s2)
merge e1            e2             = Sequence [e1, e2]

defaultNormalizationOptions :: NormalizationOptions
defaultNormalizationOptions = NormalizationOptions {
  convertObjectVariableIntoObject = True,
  convertLambdaVariableIntoFunction = True,
  convertObjectLevelFunctionIntoMethod = True,
  convertObjectLevelLambdaVariableIntoMethod = True,
  convertObjectLevelVariableIntoAttribute = True,
  sortSequenceDeclarations = SortAllNonVarables,
  insertImplicitReturn = False
}

normalize :: Expression -> Expression
normalize = normalizeWith defaultNormalizationOptions

normalizeWith :: NormalizationOptions -> Expression -> Expression
normalizeWith ops (Application (Send r m []) args) = Send (normalizeWith ops r) (normalizeWith ops m) (mapNormalizeWith ops args)
normalizeWith ops (Application e es)               = Application (normalizeWith ops e) (mapNormalizeWith ops es)
normalizeWith ops (Arrow e1 e2)                    = Arrow (normalizeWith ops e1) (normalizeWith ops e2)
normalizeWith ops (Attribute n e)                  = Attribute n (normalizeWith ops e)
normalizeWith _   (Fact n args)                    = Fact n args
normalizeWith ops (For stms e1)                    = For stms (normalizeWith ops e1)
normalizeWith ops (Forall e1 e2)                   = Forall (normalizeWith ops e1) (normalizeWith ops e2)
normalizeWith ops (ForLoop init cond prog stmt)    = ForLoop (normalizeWith ops init) (normalizeWith ops cond) (normalizeWith ops prog) (normalizeWith ops stmt)
normalizeWith ops (Function n equations)           = Function n (mapNormalizeEquationWith ops equations)
normalizeWith ops (If e1 e2 e3)                    = If (normalizeWith ops e1) (normalizeWith ops e2) (normalizeWith ops e3)
normalizeWith ops (Lambda ps e2)                   = Lambda ps (normalizeWith ops e2)
normalizeWith ops (Match e1 equations)             = Match (normalizeWith ops e1) (mapNormalizeEquationWith ops equations)
normalizeWith ops (Method n equations)             = Method n (mapNormalizeEquationWith ops equations)
normalizeWith ops (MuDict e)                       = MuDict (normalizeWith ops e)
normalizeWith ops (MuList es)                      = MuList (mapNormalizeWith ops es)
normalizeWith ops (MuObject e)                     = MuObject (normalizeWith ops e)
normalizeWith ops (MuTuple es)                     = MuTuple (mapNormalizeWith ops es)
normalizeWith ops (Not e)                          = Not (normalizeWith ops e)
normalizeWith ops (Object n e)                     = Object n (normalizeObjectLevelWith ops e)
normalizeWith ops (Procedure n equations)          = Procedure n (mapNormalizeEquationWith ops equations)
normalizeWith ops (Return e)                       = Return (normalizeWith ops e)
normalizeWith ops (Rule n args es)                 = Rule n args (mapNormalizeWith ops es)
normalizeWith ops (Send r e es)                    = Send (normalizeWith ops r) (normalizeWith ops e) (mapNormalizeWith ops es)
normalizeWith ops (Sequence es)                    = Sequence . sortDeclarationsWith ops .  mapNormalizeWith ops $ es
normalizeWith ops (LValue n (Lambda vars e))       | convertLambdaVariableIntoFunction ops = SimpleFunction n vars (normalizeWith ops e)
normalizeWith ops (LValue n (MuObject e))          | convertObjectVariableIntoObject ops = Object n (normalizeObjectLevelWith ops e)
normalizeWith ops (Variable n e)                   = Variable n (normalizeWith ops e)
normalizeWith ops (Constant n e)                   = Constant n (normalizeWith ops e)
normalizeWith ops (Other c (Just e))               = Other c (Just (normalizeWith ops e))
normalizeWith ops (Mostly c e)                     = Mostly c (normalizeWith ops e)
normalizeWith ops (While e1 e2)                    = While (normalizeWith ops e1) (normalizeWith ops e2)
normalizeWith _ e = e

mapNormalizeWith ops = map (normalizeWith ops)
mapNormalizeEquationWith ops = map (normalizeEquationWith ops)

normalizeObjectLevelWith :: NormalizationOptions -> Expression -> Expression
normalizeObjectLevelWith ops (Function n eqs)             | convertObjectLevelFunctionIntoMethod ops       = Method n (mapNormalizeEquationWith ops eqs)
normalizeObjectLevelWith ops (LValue n (Lambda vars e))   | convertObjectLevelLambdaVariableIntoMethod ops = SimpleMethod n vars (normalizeWith ops e)
normalizeObjectLevelWith ops (LValue n e)                 | convertObjectLevelVariableIntoAttribute ops    = Attribute n (normalizeWith ops e)
normalizeObjectLevelWith ops (Sequence es)                = Sequence (map (normalizeObjectLevelWith ops) es)
normalizeObjectLevelWith ops (Mostly c e)                 = Mostly c (normalizeWith ops e)
normalizeObjectLevelWith ops e                            = normalizeWith ops e

normalizeEquationWith :: NormalizationOptions -> Equation -> Equation
normalizeEquationWith ops (Equation ps (UnguardedBody e))   = Equation ps (UnguardedBody (normalizeBodyWith ops e))
normalizeEquationWith ops (Equation ps (GuardedBody b))     = Equation ps (GuardedBody (map (\(c, e) -> (normalizeWith ops c, normalizeBodyWith ops e)) b))

normalizeBodyWith :: NormalizationOptions -> Expression -> Expression
normalizeBodyWith ops = normalizeReturnWith ops . normalizeWith ops

normalizeReturnWith :: NormalizationOptions -> Expression -> Expression
normalizeReturnWith ops e             | not $ insertImplicitReturn ops = e
normalizeReturnWith _   e             | isImplicitReturn e = Return e
normalizeReturnWith _   (Sequence es) | Just (i, l) <- unwind es, isImplicitReturn l = Sequence $ i ++ [Return l]
normalizeReturnWith _   e             = e

isImplicitReturn :: Expression -> Bool
isImplicitReturn (Reference _)         = True
isImplicitReturn (TypeCast _ _)        = True
isImplicitReturn (FieldReference _ _ ) = True
isImplicitReturn (Application _ _ )    = True
isImplicitReturn (Send _ _ _ )         = True
isImplicitReturn (New _ _ )            = True
isImplicitReturn (If _ _ _)            = True
isImplicitReturn e                     = isLiteral e


isSafeDeclaration :: Expression -> Bool
isSafeDeclaration (Attribute _ _) = False
isSafeDeclaration (LValue _ _)    = False
isSafeDeclaration (Other _ e)     = False
isSafeDeclaration (Mostly _ e)    = isSafeDeclaration e
isSafeDeclaration e               = isDeclaration e

isDeclaration :: Expression -> Bool
isDeclaration = not.null.declarators

sortDeclarationsWith :: NormalizationOptions -> [Expression] -> [Expression]
sortDeclarationsWith ops expressions | shouldSort (sortSequenceDeclarations ops) = sort expressions
                                     | otherwise                                 = expressions
  where
    shouldSort :: SequenceSortMode -> Bool
    shouldSort SortNothing             = False
    shouldSort SortUniqueNonVariables  = all isSafeDeclaration expressions && identifiersAreUnique expressions
    shouldSort SortAllNonVarables      = all isSafeDeclaration expressions
    shouldSort SortAll                 = all isDeclaration expressions

    identifiersAreUnique = unique . map declaredIdentifiers

    unique xs = nub xs == xs
