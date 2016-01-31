module Language.Mulang.Builder (compact, normalize) where

import Language.Mulang

compact :: [Expression] -> Expression
compact []  = MuNull
compact [e] = e
compact es  = Sequence es

normalize :: Expression -> Expression
normalize (VariableDeclaration n (MuObject e))    = ObjectDeclaration n (normalize e)
normalize (VariableDeclaration n (Lambda vars e)) = FunctionDeclaration n [Equation vars (UnguardedBody (normalize e))]
normalize (VariableDeclaration n e)               = VariableDeclaration n (normalize e)
normalize (FunctionDeclaration n equations)       = FunctionDeclaration n (map normalizeEquations equations)
normalize (ProcedureDeclaration n equations)      = ProcedureDeclaration n (map normalizeEquations equations)
normalize (MethodDeclaration n equations)         = MethodDeclaration n (map normalizeEquations equations)
normalize (AttributeDeclaration n e)              = AttributeDeclaration n (normalize e)
normalize (ObjectDeclaration n e)                 = ObjectDeclaration n (normalize e)
normalize (Application e es)                      = Application (normalize e) (map normalize es)
normalize (Send r e es)                           = Send (normalize r) (normalize e) (map normalize es)
normalize (Lambda ps e2)                          = Lambda ps (normalize e2)
normalize (If e1 e2 e3)                           = If (normalize e1) (normalize e2) (normalize e3)
normalize (While e1 e2)                           = While (normalize e1) (normalize e2)
normalize (Match e1 equations)                    = Match (normalize e1) (map normalizeEquations equations)
normalize (Comprehension e1 stms)                 = Comprehension (normalize e1) stms
normalize (Return e)                              = Return (normalize e)
normalize (Sequence es)                           = Sequence (map normalize es)
normalize (MuObject e)                            = MuObject (normalize e)
normalize (MuTuple es)                            = MuTuple (map normalize es)
normalize (MuList es)                             = MuList (map normalize es)
normalize e = e

normalizeEquations :: Equation -> Equation
normalizeEquations (Equation ps (UnguardedBody e))   = Equation ps (UnguardedBody (normalize e))
normalizeEquations (Equation ps (GuardedBody b))     = Equation ps (GuardedBody (map (\(c, e) -> (normalize c, normalize e)) b))


