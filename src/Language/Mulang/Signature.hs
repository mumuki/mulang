module Language.Mulang.Signature (
  arity,
  name,
  nameAndArity,
  parameterNames,
  signatureOf,
  signaturesOf,
  codeSignaturesOf,
  styledCodeSignaturesOf,
  haskellStyle,
  prologStyle,
  untypedCStyle,
  Signature(..)) where

import Language.Mulang.Binding
import Language.Mulang.Ast
import Language.Mulang.Explorer (declarationsOf)

import Data.List (transpose, intercalate, nub, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (msum)
import Data.Function (on)

type SignatureStyle = [Signature] -> [String]

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

nameAndArity :: Signature -> (Binding, Int)
nameAndArity signature = (name signature, arity signature)

parameterNames :: Signature -> [Maybe Binding]
parameterNames (AritySignature _ arity) = replicate arity Nothing
parameterNames (TypedSignature _ types) = map (const Nothing) types
parameterNames (NamedSignature _ names) = names

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
codeSignaturesOf = styledCodeSignaturesOf mulangStyle

styledCodeSignaturesOf :: SignatureStyle -> Expression -> [String]
styledCodeSignaturesOf style = style . signaturesOf

-- Styles

mulangStyle :: SignatureStyle
mulangStyle = makeLines "--" (return.s)
  where s :: Signature -> String
        s (AritySignature name arity) = name ++ "/" ++ show arity
        s (NamedSignature name names) = name ++ "(" ++ (intercalate ", " . makeParamNames $ names) ++ ")"
        s (TypedSignature name types) = name ++ " :: " ++ (intercalate " -> " types)

untypedCStyle :: SignatureStyle
untypedCStyle = makeLines "//" s
  where s (AritySignature name 0)     = Just name
        s (NamedSignature name names) = Just $ name ++ "(" ++ (intercalate ", " . makeParamNames $ names) ++ ")"
        s _                           = Nothing

haskellStyle :: SignatureStyle
haskellStyle = groupAndMakeLinesOn "--" name s
  where s (NamedSignature name names) = Just $ name ++ " " ++ (intercalate " " . makeParamNames $ names)
        s (TypedSignature name types) = Just $ name ++ " :: " ++ (intercalate " -> " types)
        s (AritySignature name 0)     = Just name
        s _                           = Nothing

prologStyle :: SignatureStyle
prologStyle = groupAndMakeLinesOn "%%" nameAndArity s
  where s (AritySignature name arity) = Just $ name ++ "/" ++ show arity
        s _                           = Nothing

--Style helper functions

makeParamNames = map (fromMaybe "?")
makeLines comment f = prefixSignaturesWithComment comment . mapMaybe f
groupAndMakeLinesOn comment f s = map (intercalate "\n" . makeLines comment s) . groupSignaturesOn f
prefixSignaturesWithComment comment = map (\s -> comment ++ " " ++ s)
groupSignaturesOn f = groupBy ((==) `on` f)
