module Language.Mulang.Signature (
  arity,
  name,
  paramterNames,
  Signature(..)) where

import Language.Mulang.Binding

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