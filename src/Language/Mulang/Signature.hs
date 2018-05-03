module Language.Mulang.Signature (
  arity,
  name,
  nameAndArity,
  parameterNames,
  signatureOf,
  signaturesOf,
  codeSignaturesOf,
  styledCodeSignaturesOf,
  mulangStyle,
  haskellStyle,
  prologStyle,
  untypedCStyle,
  Signature(..),
  SignatureStyle) where

import Language.Mulang.Identifier
import Language.Mulang.Ast
import Language.Mulang.Generator (declarations)

import Data.List (transpose, intercalate, nub, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad (msum)
import Data.Function (on)

type SignatureStyle = [Signature] -> [String]

data Signature = AritySignature Identifier Int
               | TypedSignature Identifier [Identifier] Identifier
               | NamedSignature Identifier [Maybe Identifier] deriving (Show, Eq)


arity :: Signature -> Int
arity (AritySignature _ a)    = a
arity (TypedSignature _ ps _) = length ps
arity (NamedSignature _ ps)   = length ps

name :: Signature -> Identifier
name (AritySignature n _)   = n
name (TypedSignature n _ _) = n
name (NamedSignature n _)   = n

nameAndArity :: Signature -> (Identifier, Int)
nameAndArity signature = (name signature, arity signature)

parameterNames :: Signature -> [Maybe Identifier]
parameterNames (AritySignature _ arity)   = replicate arity Nothing
parameterNames (TypedSignature _ types _) = map (const Nothing) types
parameterNames (NamedSignature _ names)   = names

signaturesOf :: Expression -> [Signature]
signaturesOf = nub . mapMaybe (signatureOf.snd) . declarations

signatureOf :: Expression -> Maybe Signature
signatureOf (Subroutine name es)                  = Just $ NamedSignature name (parameterNamesOf es)
signatureOf (Clause name args _)                  = Just $ AritySignature name (length args)
signatureOf (TypeSignature name t)                = typedSignatureOf name t
signatureOf (Variable name _)                     = Just $ AritySignature name 0
signatureOf _                                     = Nothing

typedSignatureOf :: Identifier -> Type -> Maybe Signature
typedSignatureOf name (ParameterizedType args ret [])  = Just $ TypedSignature name args ret
typedSignatureOf name (SimpleType ret [])              = Just $ TypedSignature name [] ret
typedSignatureOf _ _                                   = Nothing

parameterNamesOf :: [Equation] -> [Maybe Identifier]
parameterNamesOf = map msum . transpose . map (map parameterNameOf . equationPatterns)

parameterNameOf :: Pattern -> Maybe Identifier
parameterNameOf (VariablePattern v) = Just v
parameterNameOf _                   = Nothing

codeSignaturesOf :: Expression -> [Code]
codeSignaturesOf = styledCodeSignaturesOf mulangStyle

styledCodeSignaturesOf :: SignatureStyle -> Expression -> [Code]
styledCodeSignaturesOf style = style . signaturesOf

-- Styles

mulangStyle :: SignatureStyle
mulangStyle = makeLines "--" (return.s)
  where s :: Signature -> String
        s (AritySignature name arity)     = name ++ "/" ++ show arity
        s (NamedSignature name names)     = name ++ "(" ++ (intercalate ", " . makeParamNames $ names) ++ ")"
        s (TypedSignature name types ret) = name ++ "(" ++ (intercalate ", " types) ++ "): " ++ ret

untypedCStyle :: SignatureStyle
untypedCStyle = makeLines "//" s
  where s (AritySignature name 0)     = Just name
        s (NamedSignature name names) = Just $ name ++ "(" ++ (intercalate ", " . makeParamNames $ names) ++ ")"
        s _                           = Nothing

haskellStyle :: SignatureStyle
haskellStyle = groupAndMakeLinesOn "--" name s
  where s (NamedSignature name names)     = Just $ name ++ " " ++ (intercalate " " . makeParamNames $ names)
        s (TypedSignature name types ret) = Just $ name ++ " :: " ++ (intercalate " -> " (types ++ [ret]))
        s (AritySignature name 0)         = Just name
        s _                               = Nothing

prologStyle :: SignatureStyle
prologStyle = groupAndMakeLinesOn "%%" nameAndArity s
  where s (AritySignature name arity) = Just $ name ++ "/" ++ show arity
        s _                           = Nothing

--Style helper functions

makeParamNames :: [Maybe String] -> [String]
makeParamNames = map (fromMaybe "?")

makeLines :: String -> (Signature -> Maybe String) -> [Signature] -> [String]
makeLines comment f = prefixCodeSignaturesWithComment comment . mapMaybe f

groupAndMakeLinesOn :: Eq a => String -> (Signature -> a) -> (Signature -> Maybe String) -> [Signature] -> [String]
groupAndMakeLinesOn comment groupFunction s = map (intercalate "\n" . makeLines comment s) . groupSignaturesOn groupFunction

prefixCodeSignaturesWithComment :: String -> [String] -> [String]
prefixCodeSignaturesWithComment comment = map (\s -> comment ++ " " ++ s)

groupSignaturesOn :: Eq a => (Signature -> a) -> [Signature] -> [[Signature]]
groupSignaturesOn f = groupBy ((==) `on` f)
