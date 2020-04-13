{-# LANGUAGE DeriveGeneric #-}

-- | The Ast module describes a generic, abstract language AST.
-- |
-- | Since it only describes the AST, but not many aspects of its semantics, Mulang is non-computable, but just a target
-- | for code analysis.
-- |
-- | Mulang AST itself is the combination of other abstract, paradigmatic languages that express the structures of:
-- |    * imperative programing
-- |    * object-oriented programing
-- |    * functional programing
-- |    * logic programing
-- |
module Language.Mulang.Ast (
  module Language.Mulang.Ast.Expression,
  module Language.Mulang.Ast.HighLevel
) where

import Language.Mulang.Ast.Expression
import Language.Mulang.Ast.HighLevel
