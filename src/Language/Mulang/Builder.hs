module Language.Mulang.Builder (compact, normalize) where

import Language.Mulang.Ast

compact :: [Expression] -> Expression
compact []  = MuNull
compact [e] = e
compact es  = Sequence es

normalize :: Expression -> Expression
normalize (Variable n (MuObject e))        = Object n (normalizeInObject e)
normalize (Variable n (Lambda vars e))     = SimpleFunction n vars (normalize e)
normalize (Variable n e)                   = Variable n (normalize e)
normalize (Function n equations)           = Function n (map normalizeEquation equations)
normalize (Procedure n equations)          = Procedure n (map normalizeEquation equations)
normalize (Fact n args)                    = Fact n args
normalize (Rule n args es)                 = Rule n args (map normalize es)
normalize (Method n equations)             = Method n (map normalizeEquation equations)
normalize (Attribute n e)                  = Attribute n (normalize e)
normalize (Object n e)                     = Object n (normalizeInObject e)
normalize (Application (Send r m []) args) = Send (normalize r) (normalize m) (map normalize args)
normalize (Application e es)               = Application (normalize e) (map normalize es)
normalize (Send r e es)                    = Send (normalize r) (normalize e) (map normalize es)
normalize (Lambda ps e2)                   = Lambda ps (normalize e2)
normalize (If e1 e2 e3)                    = If (normalize e1) (normalize e2) (normalize e3)
normalize (While e1 e2)                    = While (normalize e1) (normalize e2)
normalize (Match e1 equations)             = Match (normalize e1) (map normalizeEquation equations)
normalize (Comprehension e1 stms)          = Comprehension (normalize e1) stms
normalize (Return e)                       = Return (normalize e)
normalize (Not e)                          = Not (normalize e)
normalize (Forall e1 e2)                   = Forall (normalize e1) (normalize e2)
normalize (Sequence es)                    = Sequence (map normalize es)
normalize (MuObject e)                     = MuObject (normalize e)
normalize (MuTuple es)                     = MuTuple (map normalize es)
normalize (MuList es)                      = MuList (map normalize es)
normalize e = e

normalizeInObject (Function n eqs)             = Method n (map normalizeEquation eqs)
normalizeInObject (Variable n (Lambda vars e)) = SimpleMethod n vars (normalize e)
normalizeInObject (Variable n e)               = Attribute n (normalize e)
normalizeInObject (Sequence es)                = Sequence (map normalizeInObject es)
normalizeInObject e                            = normalize e

normalizeEquation :: Equation -> Equation
normalizeEquation (Equation ps (UnguardedBody e))   = Equation ps (UnguardedBody (normalize e))
normalizeEquation (Equation ps (GuardedBody b))     = Equation ps (GuardedBody (map (\(c, e) -> (normalize c, normalize e)) b))
