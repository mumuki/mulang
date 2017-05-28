module Language.Mulang.Signature (
  arity,
  name,
  code,
  paramterNames,
  signatureOf,
  signaturesOf,
  codeSignaturesOf,
  Signature(..)) where

import Language.Mulang.Binding
import Language.Mulang.Ast
import Language.Mulang.Explorer (declarationsOf)

import           Data.List (transpose, intercalate, nub)
import           Data.Maybe (mapMaybe)
import           Control.Monad (msum)

data Signature = AritySignature Binding Int
               | TypedSignature Binding [Binding]
               | NamedSignature Binding [Maybe Binding] deriving (Show, Eq)


arity :: Signature -> Int
arity (AritySignature _ a) = a
arity (TypedSignature _ ps) = length ps
arity (NamedSignature _ ps) = length ps

name :: Signature -> Binding
name (AritySignature n _) = n
name (TypedSignature n _) = n
name (NamedSignature n _) = n

paramterNames :: Signature -> [Maybe Binding]
paramterNames (AritySignature _ arity) = replicate arity Nothing
paramterNames (TypedSignature _ types) = map (const Nothing) types
paramterNames (NamedSignature _ names) = names

signaturesOf :: Expression -> [Signature]
signaturesOf = nub . mapMaybe (signatureOf.snd) . declarationsOf

signatureOf :: Expression -> Maybe Signature
signatureOf (FunctionDeclaration name equations)  = Just $ NamedSignature name (parameterNamesOf equations)
signatureOf (ProcedureDeclaration name equations) = Just $ NamedSignature name (parameterNamesOf equations)
signatureOf (RuleDeclaration name args _)         = Just $ AritySignature name (length args)
signatureOf (FactDeclaration name args)           = Just $ AritySignature name (length args)
signatureOf (TypeSignature name args)             = Just $ TypedSignature name args
signatureOf (VariableDeclaration name _)          = Just $ AritySignature name 0
signatureOf _                                     = Nothing

parameterNamesOf :: [Equation] -> [Maybe Binding]
parameterNamesOf = map msum . transpose . map (map parameterNameOf . equationParams)

parameterNameOf :: Pattern -> Maybe Binding
parameterNameOf (VariablePattern v) = Just v
parameterNameOf _                   = Nothing

codeSignaturesOf :: Expression -> [String]
codeSignaturesOf = map code . signaturesOf

code :: Signature -> String
code (AritySignature name arity) = name ++ "/" ++ show arity
code (NamedSignature name names) = name ++ "(" ++ (intercalate ", " . zipWith makeName [1..] $ names) ++ ")"
code (TypedSignature name types) = name ++ "::" ++ (intercalate " -> " types)

makeName :: Int -> Maybe Binding -> String
makeName _        (Just name) = name
makeName position Nothing     = "arg" ++ show position