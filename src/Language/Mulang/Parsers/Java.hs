{-# LANGUAGE ViewPatterns #-}


module Language.Mulang.Parsers.Java (java, parseJava) where

import Language.Mulang.Ast hiding (Primitive, While, Return, Lambda, Try, Switch, Assert, Modifier(..))
import qualified Language.Mulang.Ast as M
import qualified Language.Mulang.Ast.Operator as O
import Language.Mulang.Parsers
import Language.Mulang.Builder (compact, compactMap, compactConcatMap)

import Language.Java.Parser
import Language.Java.Syntax
import Language.Java.Pretty (prettyPrint)

import Control.Fallible

import Data.Maybe (fromMaybe)
import Data.List (intercalate, partition)
import Data.List.Extra (headOrElse, dropLast)
import Data.Char (toLower)

java :: Parser
java = orFail . parseJava'

parseJava :: EitherParser
parseJava = orLeft . parseJava'

parseJava' = fmap m . j

m (CompilationUnit _ _ typeDecls) = compactMap muTypeDecl $ typeDecls

muTypeDecl (ClassTypeDecl decl)     = muClassTypeDecl decl
muTypeDecl (InterfaceTypeDecl decl) = muInterfaceTypeDecl decl

muClass (ClassDecl _ name _ superclass interfaces (ClassBody body)) =
  Class (i name) (fmap muRefType superclass) (compact (map muImplements interfaces ++ concatMap muDecl body))

muInterface (InterfaceDecl _ name _ interfaces (InterfaceBody body)) =
  Interface (i name) (map muRefType interfaces) (compactConcatMap muMemberDecl body)

muClassTypeDecl clazz@(ClassDecl modifiers name args _ _ _) = decorate modifiers . muDeclaration name args $ muClass clazz
muClassTypeDecl (EnumDecl modifiers name _ (EnumBody constants _)) =
  decorate modifiers $ Enumeration (i name) (map muEnumConstant constants)

muImplements interface = Implement $ Reference (muRefType interface)

muInterfaceTypeDecl interface@(InterfaceDecl _ name args _ _) = muDeclaration name args $ muInterface interface

muDeclaration _ [] decl = decl
muDeclaration name args decl = Sequence [ModuleSignature (i name) (map prettyPrint args), decl]

muDecl :: Decl -> [Expression]
muDecl (MemberDecl memberDecl) = muMemberDecl memberDecl
muDecl (InitDecl _ block)      = [muBlock block]

muMemberDecl :: MemberDecl -> [Expression]
muMemberDecl (FieldDecl modifiers typ varDecls)                               = decorateMany modifiers . concatMap (variableToAttribute.muVarDecl typ) $ varDecls
muMemberDecl (MethodDecl (elem (Annotation (MarkerAnnotation (Name [Ident "Test"]))) -> True) _ Nothing (Ident name) [] _ body)
                                                                              = return $ Test (MuString name) (muMethodBody body)
muMemberDecl (MethodDecl (elem Static -> True) _ Nothing (Ident "main") [_] _ body)
                                                                              = return $ EntryPoint "main" (muMethodBody body)
muMemberDecl (MethodDecl modifiers typeParams typ name params _ (MethodBody Nothing))
                                                                              = return $ decorate modifiers $ muMethodSignature name params typ typeParams
muMemberDecl (MethodDecl (elem Public -> True) _ _ (Ident "equals") params _ body)
                                                                              = return $ PrimitiveMethod O.Equal [SimpleEquation (map muFormalParam params) (muMethodBody body)]
muMemberDecl (MethodDecl (elem Public -> True) _ _ (Ident "hashCode") params _ body)
                                                                              = return $ PrimitiveMethod O.Hash [SimpleEquation (map muFormalParam params) (muMethodBody body)]
muMemberDecl (MethodDecl modifiers typeParams returnType name params _ body)  = decorateMany modifiers [
                                                                                  muMethodSignature name params returnType typeParams,
                                                                                  SimpleMethod (i name) (map muFormalParam params) (muMethodBody body)]
muMemberDecl e@(ConstructorDecl _ _ _ _params _ _constructorBody)             = return . debug $ e
muMemberDecl (MemberClassDecl decl)                                           = return $ muClassTypeDecl decl
muMemberDecl (MemberInterfaceDecl decl)                                       = return $ muInterfaceTypeDecl decl

muMethodSignature name params returnType typeParams = SubroutineSignature (i name) (map muFormalParamType params) (muReturnType returnType) (map muTypeParam typeParams)
muTypeParam (TypeParam (Ident i) _) = i

decorate :: [Modifier] -> Expression -> Expression
decorate ms = head . decorateMany ms . return

decorateMany :: [Modifier] -> [Expression] -> [Expression]
decorateMany modifiers es | null nonPublicModifiers = es
                          | otherwise = [Decorator (map muModifier nonPublicModifiers) (compact es)]
  where
    nonPublicModifiers = filter (/=Public) modifiers

muModifier :: Modifier -> M.Modifier
muModifier Static                                        = M.Static
muModifier Abstract                                      = M.Abstract
muModifier Private                                       = M.Private
muModifier Protected                                     = M.Protected
muModifier (Annotation (MarkerAnnotation (Name [name]))) = M.Annotation (Reference (i name))
muModifier other                                         = debugModifier other

muEnumConstant (EnumConstant name _ _) = i name

muFormalParam (FormalParam _ _ _ id)      = VariablePattern (v id)
muFormalParamType (FormalParam _ typ _ _) = (muType typ)

muBlock (Block statements) = compactConcatMap muBlockStmt statements

muBlockStmt (BlockStmt stmt) = [muStmt stmt]
muBlockStmt (LocalClass decl) = [muClassTypeDecl decl]
muBlockStmt (LocalVars _ typ vars) = concatMap (muVarDecl typ) vars

muType (PrimType t) = muPrimType t
muType (RefType t)  = muRefType t
muReturnType = fromMaybe "void" . fmap muType

muStmt (StmtBlock block)               = muBlock block
muStmt (IfThen exp ifTrue)             = If (muExp exp) (muStmt ifTrue) None
muStmt (IfThenElse exp ifTrue ifFalse) = If (muExp exp) (muStmt ifTrue) (muStmt ifFalse)
muStmt (While cond body)               = M.While (muExp cond) (muStmt body)
muStmt (Do body cond)                  = M.While (muStmt body) (muExp cond)
muStmt (Return exp)                    = M.Return $ fmapOrNone muExp exp
muStmt (ExpStmt exp)                   = muExp exp
muStmt Empty                           = None
muStmt (Synchronized _ block)          = muBlock block
muStmt (Labeled _ stmt)                = muStmt stmt
muStmt (Throw exp)                     = Raise $ muExp exp
muStmt (Try block catches finally)     = M.Try (muBlock block) (map muCatch catches) (fmapOrNone muBlock finally)
muStmt (BasicFor init cond prog stmt)  = ForLoop (fmapOrNone muForInit init) (fmapOrNone muExp cond) (fmapOrNone (compactMap muExp) prog) (muStmt stmt)
muStmt (EnhancedFor _ _ name gen body) = For [Generator (VariablePattern (i name)) (muExp gen)] (muStmt body)
muStmt (Switch exp cases)              = muSwitch exp . partition isDefault $ cases
muStmt e                               = debug e

muExp (Lit lit)                         = muLit lit
muExp (PreMinus (Lit lit))              = preMinus $ muLit lit
muExp (MethodInv invoke)                = muMethodInvocation invoke
muExp This                              = Self
muExp (BinOp arg1 op arg2)              = Send (muExp arg1) (muOp op) [muExp arg2]
muExp (Cond cond ifTrue ifFalse)        = If (muExp cond) (muExp ifTrue) (muExp ifFalse)
muExp (ExpName name)                    = muName name
muExp (Assign lhs EqualA exp)           = muAssignment lhs (muExp exp)
muExp (InstanceCreation _ clazz args _) = New (Reference $ r clazz) (map muExp args)
muExp (PreNot exp)                      | PrimitiveSend r O.Equal [a] <- (muExp exp) = PrimitiveSend r O.NotEqual [a]
                                        | otherwise = PrimitiveSend (muExp exp) O.Negation []
muExp (Lambda params exp)               = M.Lambda (muLambdaParams params) (muLambdaExp exp)
muExp (MethodRef _ message)             = M.Lambda [VariablePattern "it"] (SimpleSend (Reference "it") (i message) [])
muExp e                                 = debug e

muLambdaExp (LambdaExpression exp) = muExp exp
muLambdaExp (LambdaBlock block) = muBlock block

muLambdaParams (LambdaSingleParam name)     = [VariablePattern (i name)]
muLambdaParams (LambdaInferredParams names) = map (VariablePattern . i) names
muLambdaParams (LambdaFormalParams params)  = map muFormalParam params

muCatch :: Catch -> (Pattern, Expression)
muCatch (Catch param block) = (TypePattern (muFormalParamType param), muBlock block)

muAssignment (FieldLhs (PrimaryFieldAccess This name)) exp = FieldAssignment Self (i name) exp
muAssignment (NameLhs (Name [name])) exp                   = Assignment (i name) exp
muAssignment (NameLhs (Name ns)) exp                       = FieldAssignment r f exp
  where (r, f) = foldReferences ns

muName (Name [name]) = Reference (i name)
muName (Name ns)     = FieldReference r f
  where (r, f) = foldReferences ns

foldReferences = foldReferences' . map i

foldReferences' :: [Identifier] -> (Expression, Identifier)
foldReferences' (n:ns) = (foldl (\a e -> FieldReference a e) (Reference n) (init ns), last ns)

muLit (String s)  = MuString s
muLit (Char c)    = MuChar c
muLit (Int i)     = MuNumber (fromIntegral i)
muLit (Float d)   = MuNumber d
muLit (Double d)  = MuNumber d
muLit (Boolean b) = MuBool   b
muLit Null        = MuNil
muLit e           = debug e

preMinus (MuNumber n) = MuNumber (negate n)
preMinus other        = other

muOp Add    = M.Primitive O.Plus
muOp And    = M.Primitive O.BitwiseAnd
muOp CAnd   = M.Primitive O.And
muOp COr    = M.Primitive O.Or
muOp Div    = M.Primitive O.Divide
muOp Equal  = M.Primitive O.Same
muOp GThan  = M.Primitive O.GreaterThan
muOp GThanE = M.Primitive O.GreaterOrEqualThan
muOp LShift = M.Primitive O.BitwiseLeftShift
muOp LThan  = M.Primitive O.LessThan
muOp LThanE = M.Primitive O.LessOrEqualThan
muOp Mult   = M.Primitive O.Multiply
muOp NotEq  = M.Primitive O.NotSame
muOp Or     = M.Primitive O.BitwiseOr
muOp Rem    = M.Primitive O.Modulo
muOp RShift = M.Primitive O.BitwiseRightShift
muOp Sub    = M.Primitive O.Minus
muOp Xor    = M.Primitive O.BitwiseXor
muOp e      = debug e

muVarDecl typ (VarDecl id init) = [
      TypeSignature (v id) (SimpleType (muType typ) []),
      Variable (v id) (fmapOrNone muVarInit init)]

muMethodBody (MethodBody (Just block)) = muBlock block

muVarInit (InitExp exp) = muExp exp
muVarInit e             = debug e

muMethodInvocation (MethodCall (Name [Ident "System", Ident "out", Ident "println"]) [expr])  = Print (muExp expr)
muMethodInvocation (MethodCall (Name [Ident "System", Ident "out", Ident "print"]) [expr])    = Print (muExp expr)
muMethodInvocation (MethodCall (Name [Ident "System", Ident "out", Ident "printf"]) (expr:_)) = Print (muExp expr)

muMethodInvocation (MethodCall (Name [message]) args)           = muNormalizeReference $ SimpleSend Self (i message) (map muExp args)
muMethodInvocation (MethodCall (Name receptorAndMessage) args)  = muNormalizeReference $ SimpleSend (Reference  (ns . init $ receptorAndMessage)) (i . last $ receptorAndMessage) (map muExp args)
muMethodInvocation (PrimaryMethodCall receptor _ selector args) = muNormalizeReference $ SimpleSend (muExp receptor) (i selector) (map muExp args)
muMethodInvocation e = debug e

muNormalizeReference (SimpleSend Self "assertTrue" [expression])         = M.Assert False $ Truth expression
muNormalizeReference (SimpleSend Self "assertFalse" [expression])        = M.Assert True $ Truth expression
muNormalizeReference (SimpleSend Self "assertEquals" [expected, actual]) = M.Assert False $ Equality expected actual
muNormalizeReference (SimpleSend one  "equals" [other])                  = M.PrimitiveSend one O.Equal [other]
muNormalizeReference (SimpleSend one  "hashCode" [other])                = M.PrimitiveSend one O.Hash [other]
muNormalizeReference e = e

muRefType (ClassRefType clazz) = r clazz
muRefType (ArrayType t)        = (muType t) ++ "[]"

muPrimType = map toLower . dropLast 1 . show

muSwitch exp (def, cases) =  M.Switch (muExp exp) (map muCase cases) (headOrElse None . map muDefault $ def)

muCase (SwitchBlock (SwitchCase exp) block) = (muExp exp, compactConcatMap muBlockStmt block)

muDefault (SwitchBlock Default block) = compactConcatMap muBlockStmt block

muForInit:: ForInit -> Expression
muForInit (ForLocalVars _ typ varDecls) = compactConcatMap (muVarDecl typ) varDecls
muForInit (ForInitExps exps) = compactMap muExp exps

isDefault (SwitchBlock Default _) = True
isDefault _                       = False

-- Combinators

fmapOrNone f = fromMaybe None . fmap f

-- Helpers

variableToAttribute [typ, (Variable id init)] = [typ, Attribute id init]

v (VarId name) = i name
v (VarDeclArray id) =  (v id) ++ "[]"

i (Ident name) = name
r (ClassType [(name, _)]) = i name

j = parser compilationUnit

ns :: [Ident] -> String
ns = intercalate "." . map i
