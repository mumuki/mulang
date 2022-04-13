{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Ast.Modifier (
  Modifier(..)) where

import           GHC.Generics

data Modifier
    = Abstract
    | Static
    | Private
    | Protected
    | OtherModifier
  deriving (Eq, Show, Read, Generic, Ord, Enum)
