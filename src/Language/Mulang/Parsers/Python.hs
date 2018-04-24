module Language.Mulang.Parsers.Python (py, parsePython) where

import qualified Language.Mulang.Ast as M 
import Language.Mulang.Builder
import Language.Mulang.Parsers

import Language.Python.Version3.Parser (parseModule)
import Language.Python.Common.Token (Token)
import Language.Python.Common.AST

import Control.Fallible

py:: Parser
py = orFail . parsePython'

parsePython:: EitherParser
parsePython = orLeft . parsePython'

parsePython' = fmap (normalize . muPyAST) . (`parseModule` "")
