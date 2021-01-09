{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts, FlexibleInstances #-}

module Language.Mulang.Serializer (serialize) where

import GHC.Generics

import Language.Mulang.Ast
import Language.Mulang.Ast.Operator

serialize :: Bracket a => a -> String
serialize = bracketize

instance Bracket Pattern
instance Bracket Expression
instance Bracket Equation
instance Bracket EquationBody
instance Bracket Statement
instance Bracket Operator
instance Bracket Type
instance Bracket Assertion

instance (Bracket a, Bracket b) => Bracket (a, b) where
  bracketize (a, b) = "{," ++ bracketize a ++ bracketize b ++"}"

instance Bracket a => Bracket (Maybe a) where
  bracketize Nothing = "{}"
  bracketize (Just a) = "{" ++ bracketize a ++ "}"

instance Bracket a => Bracket [a] where
  bracketize = concatMap bracketize

instance {-# OVERLAPPING #-} Bracket String where
  bracketize s = "{" ++ s ++ "}"

instance Bracket Double where
  bracketize = showBracketize

instance Bracket Bool where
  bracketize = showBracketize

instance Bracket Char where
  bracketize = showBracketize

showBracketize :: Show a => a -> String
showBracketize = bracketize . show

class Bracket' f where
  bracketize' :: f p -> String

instance Bracket' V1 where
  bracketize' _ = ""

instance Bracket' U1 where
  bracketize' _ = ""

instance Bracket c => Bracket' (K1 i c) where
  bracketize' (K1 x) = bracketize x

instance (Bracket' f, Constructor c) => Bracket' (M1 C c f) where
  bracketize' c@(M1 x) = "{" ++ (conName c) ++ bracketize' x ++ "}"

instance (Bracket' f) => Bracket' (M1 S s f) where
  bracketize' (M1 x) = bracketize' x

instance (Bracket' f) => Bracket' (M1 D d f) where
  bracketize' (M1 x) = bracketize' x

instance (Bracket' a, Bracket' b) => Bracket' (a :+: b) where
  bracketize' (L1 x) = bracketize' x
  bracketize' (R1 x) = bracketize' x

instance (Bracket' a, Bracket' b) => Bracket' (a :*: b) where
  bracketize' (a :*: b) = bracketize' a ++ bracketize' b

class Bracket a where
  bracketize :: a -> String
  default bracketize :: (Generic a, Bracket' (Rep a)) => a -> String
  bracketize = bracketize' . from
