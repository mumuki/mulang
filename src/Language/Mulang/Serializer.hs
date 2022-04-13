{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Language.Mulang.Serializer (
  bracket,
  brace) where

import GHC.Generics

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator

bracket :: Expression -> String
bracket = bracketize "[" "]"

brace :: Expression -> String
brace = bracketize "{" "}"

instance Bracket Pattern
instance Bracket Expression
instance Bracket Equation
instance Bracket EquationBody
instance Bracket Statement
instance Bracket Operator
instance Bracket Modifier
instance Bracket Type
instance Bracket Assertion

instance (Bracket a, Bracket b) => Bracket (a, b) where
  bracketize l r (a, b) = l ++ "," ++ bracketize l r a ++ bracketize l r b ++ r

instance Bracket a => Bracket (Maybe a) where
  bracketize l r Nothing = l ++ r
  bracketize l r (Just a) = l ++ bracketize l r a ++ r

instance Bracket a => Bracket [a] where
  bracketize l r = concatMap (bracketize l r)

instance {-# OVERLAPPING #-} Bracket String where
  bracketize l r s = l ++ s ++ r

instance Bracket Double where
  bracketize = showBracketize

instance Bracket Bool where
  bracketize = showBracketize

instance Bracket Char where
  bracketize = showBracketize

showBracketize :: Show a => String -> String -> a -> String
showBracketize l r = bracketize l r . show

class Bracket' f where
  bracketize' :: String -> String -> f p -> String

instance Bracket' V1 where
  bracketize' _ _ _ = ""

instance Bracket' U1 where
  bracketize' _ _ _ = ""

instance Bracket c => Bracket' (K1 i c) where
  bracketize' l r (K1 x) = bracketize l r x

instance (Bracket' f, Constructor c) => Bracket' (M1 C c f) where
  bracketize' l r c@(M1 x) = l ++ (conName c) ++ bracketize' l r x ++ r

instance (Bracket' f) => Bracket' (M1 S s f) where
  bracketize' l r (M1 x) = bracketize' l r x

instance (Bracket' f) => Bracket' (M1 D d f) where
  bracketize' l r (M1 x) = bracketize' l r x

instance (Bracket' a, Bracket' b) => Bracket' (a :+: b) where
  bracketize' l r (L1 x) = bracketize' l r x
  bracketize' l r (R1 x) = bracketize' l r x

instance (Bracket' a, Bracket' b) => Bracket' (a :*: b) where
  bracketize' l r (a :*: b) = bracketize' l r a ++ bracketize' l r b

class Bracket a where
  bracketize :: String -> String -> a -> String
  default bracketize :: (Generic a, Bracket' (Rep a)) => String -> String -> a -> String
  bracketize l r = bracketize' l r . from
