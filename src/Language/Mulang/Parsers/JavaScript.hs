module Language.Mulang.Parsers.JavaScript where

import Language.Mulang
import Language.Haskell.Syntax
import Language.Haskell.Parser

import Language.JavaScript.Parser.Parser
import Language.JavaScript.Parser.AST


parseJavaScript :: String -> Maybe Program
parseJavaScript code = Just . mu $ readJs code

mu (NN (JSSourceElementsTop staments)) =  Program (concatMap (muNode.gc) staments)
  where
    muNode (JSLiteral _) = []
    muNode (JSIdentifier n) = [Variable n]
    muNode (JSDecimal val) = [Literal (MuFloat (read val))]
    muNode (JSVariables _ decls _)  = concatMap (muNode.gc) decls
    muNode (JSVarDecl (NT (JSIdentifier var) _ _) initial) = [
                             (DeclarationExpression . ConstantDeclaration var) (UnguardedRhs . head . concatMap (muNode . gc) $ initial)]
    muNode _ = []

    gc (NN n) = n
    gc (NT n _ _) = n

