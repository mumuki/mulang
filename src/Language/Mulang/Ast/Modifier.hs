{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Ast.Modifier (
  Modifier(..)) where

import           GHC.Generics

data Modifier
    = Abstract
    | Static
    | Private
    | Protected
    | Public
  deriving (Eq, Show, Read, Generic, Ord, Enum)
