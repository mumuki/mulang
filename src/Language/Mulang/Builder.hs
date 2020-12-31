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
import Language.Mulang.Ast.Visitor
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
normalizeWith ops (Application (Send r m []) args)      = Send (normalizeWith ops r) (normalizeWith ops m) (mapNormalize ops args)
normalizeWith ops (LValue n (Lambda vars e))            | convertLambdaVariableIntoFunction ops = SimpleFunction n vars (normalizeWith ops e)
normalizeWith ops (LValue n (MuObject e))               | convertObjectVariableIntoObject ops = Object n (normalizeObjectLevel ops e)
normalizeWith ops (Object n e)                          = Object n (normalizeObjectLevel ops e)
normalizeWith ops (Sequence es)                         = Sequence . sortDeclarationsWith ops .  mapNormalize ops $ es
--
normalizeWith _    a@(Assert _ _)                       = a
normalizeWith ops (For stms e1)                         = For stms (normalizeWith ops e1)
normalizeWith ops (ForLoop e c i b)                     = ForLoop (normalizeWith ops e) (normalizeWith ops c) (normalizeWith ops i) (normalizeWith ops b)
normalizeWith ops (Lambda ps e2)                        = Lambda ps (normalizeWith ops e2)
normalizeWith ops (Match e1 equations)                  = Match (normalizeWith ops e1) (mapNormalizeEquation ops equations)
normalizeWith ops (Rule n args es)                      = Rule n args (mapNormalize ops es)
normalizeWith ops (Send r e es)                         = Send (normalizeWith ops r) (normalizeWith ops e) (mapNormalize ops es)
normalizeWith ops (Switch v cs d)                       = Switch (normalizeWith ops v) (normalizeSwitchCases ops cs) (normalizeWith ops d)
normalizeWith ops (Try t cs f)                          = Try (normalizeWith ops t) (normalizeTryCases ops cs) (normalizeWith ops f)
--
normalizeWith _   (SinglePatternsList ps c)             = c ps
normalizeWith _   c@(Terminal)                          = c
normalizeWith ops (ExpressionAndExpressionsList e es c) = c (normalizeWith ops e) (mapNormalize ops es)
normalizeWith ops (SingleEquationsList eqs c)           = c (mapNormalizeEquation ops eqs)
normalizeWith ops (SingleExpression e c)                = c (normalizeWith ops e)
normalizeWith ops (SingleExpressionsList es c)          = c (mapNormalize ops es)
normalizeWith ops (ThreeExpressions e1 e2 e3 c)         = c (normalizeWith ops e1) (normalizeWith ops e2) (normalizeWith ops e3)
normalizeWith ops (TwoExpressions e1 e2 c)              = c (normalizeWith ops e1) (normalizeWith ops e2)

mapNormalize ops = map (normalizeWith ops)
mapNormalizeEquation ops = map (normalizeEquation ops)

normalizeObjectLevel :: NormalizationOptions -> Expression -> Expression
normalizeObjectLevel ops (Function n eqs)             | convertObjectLevelFunctionIntoMethod ops       = Method n (mapNormalizeEquation ops eqs)
normalizeObjectLevel ops (LValue n (Lambda vars e))   | convertObjectLevelLambdaVariableIntoMethod ops = SimpleMethod n vars (normalizeWith ops e)
normalizeObjectLevel ops (LValue n e)                 | convertObjectLevelVariableIntoAttribute ops    = Attribute n (normalizeWith ops e)
normalizeObjectLevel ops (Sequence es)                = Sequence (map (normalizeObjectLevel ops) es)
normalizeObjectLevel ops e                            = normalizeWith ops e

normalizeEquation :: NormalizationOptions -> Equation -> Equation
normalizeEquation ops (Equation ps (UnguardedBody e))   = Equation ps (UnguardedBody (normalizeBody ops e))
normalizeEquation ops (Equation ps (GuardedBody b))     = Equation ps (GuardedBody (map (\(c, e) -> (normalizeWith ops c, normalizeBody ops e)) b))

normalizeBody :: NormalizationOptions -> Expression -> Expression
normalizeBody ops = normalizeReturn ops . normalizeWith ops

normalizeReturn :: NormalizationOptions -> Expression -> Expression
normalizeReturn ops e             | not $ insertImplicitReturn ops = e
normalizeReturn _   e             | isImplicitReturn e = Return e
normalizeReturn _   (Sequence es) | Just (i, l) <- unwind es, isImplicitReturn l = Sequence $ i ++ [Return l]
normalizeReturn _   e             = e

normalizeTryCases    ops = map (\(p, e) -> (p, normalizeWith ops e))
normalizeSwitchCases ops = map (\(e1, e2) -> (normalizeWith ops e1, normalizeWith ops e2))

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
isSafeDeclaration (Other _ _)     = False
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
