module Language.Mulang.Builder (compact, unguardedBody, normalize) where

import Language.Mulang

compact :: [Expression] -> Expression
compact []  = MuNull
compact [e] = e
compact es  = Sequence es

unguardedBody :: [Pattern] -> Expression -> [Equation]
unguardedBody vars e = [Equation vars (UnguardedBody e)]

normalize :: Expression -> Expression
normalize (VariableDeclaration n (MuObject e))    = ObjectDeclaration n (normalizeInObject e)
normalize (VariableDeclaration n (Lambda vars e)) = FunctionDeclaration n (unguardedBody vars (normalize e))
normalize (VariableDeclaration n e)               = VariableDeclaration n (normalize e)
normalize (FunctionDeclaration n equations)       = FunctionDeclaration n (map normalizeEquation equations)
normalize (ProcedureDeclaration n equations)      = ProcedureDeclaration n (map normalizeEquation equations)
normalize (FactDeclaration n args)                = FactDeclaration n args
normalize (RuleDeclaration n args es)             = RuleDeclaration n args (map normalize es)
normalize (MethodDeclaration n equations)         = MethodDeclaration n (map normalizeEquation equations)
normalize (AttributeDeclaration n e)              = AttributeDeclaration n (normalize e)
normalize (ObjectDeclaration n e)                 = ObjectDeclaration n (normalizeInObject e)
normalize (Application (Send r m []) args)        = Send (normalize r) (normalize m) (map normalize args)
normalize (Application e es)                      = Application (normalize e) (map normalize es)
normalize (Send r e es)                           = Send (normalize r) (normalize e) (map normalize es)
normalize (Lambda ps e2)                          = Lambda ps (normalize e2)
normalize (If e1 e2 e3)                           = If (normalize e1) (normalize e2) (normalize e3)
normalize (While e1 e2)                           = While (normalize e1) (normalize e2)
normalize (Match e1 equations)                    = Match (normalize e1) (map normalizeEquation equations)
normalize (Comprehension e1 stms)                 = Comprehension (normalize e1) stms
normalize (Return e)                              = Return (normalize e)
normalize (Not e)                                 = Not (normalize e)
normalize (Forall e1 e2)                          = Forall (normalize e1) (normalize e2)
normalize (Sequence es)                           = Sequence (map normalize es)
normalize (MuObject e)                            = MuObject (normalize e)
normalize (MuTuple es)                            = MuTuple (map normalize es)
normalize (MuList es)                             = MuList (map normalize es)
normalize e = e



normalizeInObject (FunctionDeclaration n eqs)             = MethodDeclaration n (map normalizeEquation eqs)
normalizeInObject (VariableDeclaration n (Lambda vars e)) = MethodDeclaration n (unguardedBody vars (normalize e))
normalizeInObject (VariableDeclaration n e)               = AttributeDeclaration n (normalize e)
normalizeInObject (Sequence es)                           = Sequence (map normalizeInObject es)
normalizeInObject e                                       = normalize e

normalizeEquation :: Equation -> Equation
normalizeEquation (Equation ps (UnguardedBody e))   = Equation ps (UnguardedBody (normalize e))
normalizeEquation (Equation ps (GuardedBody b))     = Equation ps (GuardedBody (map (\(c, e) -> (normalize c, normalize e)) b))


