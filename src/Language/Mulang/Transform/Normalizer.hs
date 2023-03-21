{-# LANGUAGE DeriveGeneric, ViewPatterns #-}

module Language.Mulang.Transform.Normalizer (
    normalize,
    unnormalized,
    NormalizationOptions (..),
    SequenceSortMode (..)) where

import           GHC.Generics

import Data.List (sort, nub)
import Data.List.Extra (unwind)

import Language.Mulang.Ast
import Language.Mulang.Ast.Visitor
import Language.Mulang.Ast.Operator (isCommutative)
import Language.Mulang.Builder (compact, trim)
import Language.Mulang.Generator (declarators, declaredIdentifiers)
import Language.Mulang.Inspector.Literal (isLiteral)

data NormalizationOptions = NormalizationOptions {
  convertObjectVariableIntoObject :: Bool,
  convertLambdaVariableIntoFunction :: Bool,
  convertObjectLevelFunctionIntoMethod :: Bool,
  convertObjectLevelLambdaVariableIntoMethod :: Bool,
  convertObjectLevelVariableIntoAttribute :: Bool,
  convertObjectIntoDict :: Bool,
  convertProcedureByPartsIntoFunction :: Bool,
  sortSequenceDeclarations :: SequenceSortMode,
  insertImplicitReturn :: Bool,
  compactSequences :: Bool,
  trimSequences :: Bool,
  sortCommutativeApplications :: Bool
} deriving (Eq, Show, Read, Generic)

data SequenceSortMode
  = SortNothing
  | SortUniqueNonVariables
  | SortAllNonVariables
  | SortAll deriving (Eq, Show, Read, Generic)

unnormalized :: NormalizationOptions
unnormalized = NormalizationOptions {
  convertObjectVariableIntoObject = False,
  convertLambdaVariableIntoFunction = False,
  convertObjectLevelFunctionIntoMethod = False,
  convertObjectLevelLambdaVariableIntoMethod = False,
  convertObjectLevelVariableIntoAttribute = False,
  convertObjectIntoDict = False,
  convertProcedureByPartsIntoFunction = False,
  sortSequenceDeclarations = SortNothing,
  insertImplicitReturn = False,
  compactSequences = False,
  trimSequences = False,
  sortCommutativeApplications = False
}

normalize :: NormalizationOptions -> Expression -> Expression
normalize ops (Application (Send r m []) args)      = Send (normalize ops r) (normalize ops m) (mapNormalize ops args)
normalize ops (Application (Primitive op) [e1, e2]) | isCommutative op = Application (Primitive op) (normalizeCommutativeArguments ops [e1, e2])
normalize ops (LValue n (Lambda vars e))            | convertLambdaVariableIntoFunction ops = SimpleFunction n vars (normalize ops e)
normalize ops (LValue n (MuObject e))               | convertObjectVariableIntoObject ops = Object n (normalizeObjectLevel ops e)
normalize ops (MuObject e)                          | convertObjectIntoDict ops = MuDict . normalize ops . normalizeArrows $ e
normalize ops (SimpleProcedure name params body)    | convertProcedureByPartsIntoFunction ops && isBodyByParts body = SimpleFunction name params (normalize ops body)
normalize ops (Object n e)                          = Object n (normalizeObjectLevel ops e)
normalize ops (Sequence es)                         = normalizeSequence ops . sortDeclarations ops .  mapNormalize ops $ es
--
normalize _    a@(Assert _ _)                       = a
normalize ops (For stms e1)                         = For stms (normalize ops e1)
normalize ops (ForLoop e c i b)                     = ForLoop (normalize ops e) (normalize ops c) (normalize ops i) (normalize ops b)
normalize ops (Lambda ps e2)                        = Lambda ps (normalize ops e2)
normalize ops (Match e1 equations)                  = Match (normalize ops e1) (mapNormalizeEquation ops equations)
normalize ops (Rule n args es)                      = Rule n args (mapNormalize ops es)
normalize ops (Send r e es)                         = Send (normalize ops r) (normalize ops e) (mapNormalize ops es)
normalize ops (Switch v cs d)                       = Switch (normalize ops v) (normalizeSwitchCases ops cs) (normalize ops d)
normalize ops (Try t cs f)                          = Try (normalize ops t) (normalizeTryCases ops cs) (normalize ops f)
--
normalize _   (SinglePatternsList ps c)             = c ps
normalize _   c@(Terminal)                          = c
normalize ops (ExpressionAndExpressionsList e es c) = c (normalize ops e) (mapNormalize ops es)
normalize ops (SingleEquationsList eqs c)           = c (mapNormalizeEquation ops eqs)
normalize ops (SingleExpression e c)                = c (normalize ops e)
normalize ops (SingleExpressionsList es c)          = c (mapNormalize ops es)
normalize ops (ThreeExpressions e1 e2 e3 c)         = c (normalize ops e1) (normalize ops e2) (normalize ops e3)
normalize ops (TwoExpressions e1 e2 c)              = c (normalize ops e1) (normalize ops e2)

mapNormalize ops = map (normalize ops)
mapNormalizeEquation ops = map (normalizeEquation ops)

normalizeArrows :: Expression -> Expression
normalizeArrows (Sequence es) = Sequence . map normalizeArrows $ es
normalizeArrows (LValue n v)  = Arrow (MuString n) v
normalizeArrows e             = e

normalizeSequence :: NormalizationOptions -> [Expression] -> Expression
normalizeSequence ops = compact' . trim'
  where
    compact' = if compactSequences ops then compact else Sequence
    trim'    = if trimSequences ops then trim else id

normalizeCommutativeArguments :: NormalizationOptions -> [Expression] -> [Expression]
normalizeCommutativeArguments ops args | sortCommutativeApplications ops = sort args
normalizeCommutativeArguments _   args = args

normalizeObjectLevel :: NormalizationOptions -> Expression -> Expression
normalizeObjectLevel ops (Function n eqs)             | convertObjectLevelFunctionIntoMethod ops       = Method n (mapNormalizeEquation ops eqs)
normalizeObjectLevel ops (LValue n (Lambda vars e))   | convertObjectLevelLambdaVariableIntoMethod ops = SimpleMethod n vars (normalize ops e)
normalizeObjectLevel ops (LValue n e)                 | convertObjectLevelVariableIntoAttribute ops    = Attribute n (normalize ops e)
normalizeObjectLevel ops (Sequence es)                = normalizeSequence ops (map (normalizeObjectLevel ops) es)
normalizeObjectLevel ops e                            = normalize ops e

isBodyByParts (If _ ifTrue ifFalse)           = isBodyPart ifTrue && isBodyPart ifFalse
isBodyByParts (Sequence
                (reverse ->
                  (If _ ifTrue ifFalse) : _)) = isBodyPart ifTrue && isBodyPart ifFalse
isBodyByParts _                               = False

isBodyPart (Return _)            = True
isBodyPart (Sequence
              (reverse ->
                (Return _ : _))) = True
isBodyPart _                     = False

normalizeEquation :: NormalizationOptions -> Equation -> Equation
normalizeEquation ops = mapEquation (normalize ops) (normalizeBody ops)

normalizeBody :: NormalizationOptions -> Expression -> Expression
normalizeBody ops = normalizeReturn ops . normalize ops

normalizeReturn :: NormalizationOptions -> Expression -> Expression
normalizeReturn ops e             | not $ insertImplicitReturn ops = e
normalizeReturn _   e             | isImplicitReturn e = Return e
normalizeReturn _   (Sequence es) | Just (i, l) <- unwind es, isImplicitReturn l = Sequence $ i ++ [Return l]
normalizeReturn _   e             = e

normalizeTryCases    ops = map (\(p, e) -> (p, normalize ops e))
normalizeSwitchCases ops = map (\(e1, e2) -> (normalize ops e1, normalize ops e2))

isImplicitReturn :: Expression -> Bool
isImplicitReturn (Reference _)         = True
isImplicitReturn (TypeCast _ _)        = True
isImplicitReturn (FieldReference _ _ ) = True
isImplicitReturn (Application _ _ )    = True
isImplicitReturn (Send _ _ _ )         = True
isImplicitReturn (New _ _ )            = True
isImplicitReturn (If _ _ _)            = True
isImplicitReturn MuNil                 = False
isImplicitReturn e                     = isLiteral e


isSafeDeclaration :: Expression -> Bool
isSafeDeclaration (Attribute _ _) = False
isSafeDeclaration (LValue _ _)    = False
isSafeDeclaration (Other _ _)     = False
isSafeDeclaration e               = isDeclaration e

isDeclaration :: Expression -> Bool
isDeclaration = not.null.declarators

sortDeclarations :: NormalizationOptions -> [Expression] -> [Expression]
sortDeclarations ops expressions | shouldSort (sortSequenceDeclarations ops) = sort expressions
                                     | otherwise                                 = expressions
  where
    shouldSort :: SequenceSortMode -> Bool
    shouldSort SortNothing             = False
    shouldSort SortUniqueNonVariables  = all isSafeDeclaration expressions && identifiersAreUnique expressions
    shouldSort SortAllNonVariables      = all isSafeDeclaration expressions
    shouldSort SortAll                 = all isDeclaration expressions

    identifiersAreUnique = unique . map declaredIdentifiers

    unique xs = nub xs == xs
