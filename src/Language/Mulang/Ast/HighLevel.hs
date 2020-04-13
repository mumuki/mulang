{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Language.Mulang.Ast.HighLevel (
    extractUnification,
    extractSubroutine,
    extractParams,
    extractCall,
    extractClause,
    extractReference,
    pattern SimpleEquation,
    pattern SimpleFunction,
    pattern SimpleProcedure,
    pattern SimpleMethod,
    pattern SimpleSend,
    pattern PrimitiveSend,
    pattern SubroutineSignature,
    pattern VariableSignature,
    pattern ModuleSignature,
    pattern Unification,
    pattern MuTrue,
    pattern MuFalse,
    pattern Subroutine,
    pattern Clause,
    pattern Call,
    pattern Params
  ) where

import Language.Mulang.Ast.Expression

pattern VariableSignature name t cs        = TypeSignature name (SimpleType t cs)
pattern SubroutineSignature name args t cs = TypeSignature name (ParameterizedType args t cs)
pattern ModuleSignature name cs            = TypeSignature name (ConstrainedType cs)

pattern SimpleEquation params body = Equation params (UnguardedBody body)

pattern SimpleSend receptor selector args = Send receptor (Reference selector) args
pattern PrimitiveSend receptor selector args = Send receptor (Primitive selector) args

pattern SimpleFunction name params body  = Function  name [SimpleEquation params body]
pattern SimpleProcedure name params body = Procedure name [SimpleEquation params body]
pattern SimpleMethod name params body    = Method    name [SimpleEquation params body]

pattern Unification name value <- (extractUnification -> Just (name, value))

pattern MuTrue  = MuBool True
pattern MuFalse = MuBool False

pattern Subroutine name body <- (extractSubroutine -> Just (name, body))
pattern Clause name patterns expressions <- (extractClause -> Just (name, patterns, expressions))

pattern Call operation arguments <- (extractCall -> Just (operation, arguments))

pattern Params params <- (extractParams -> Just params)

extractUnification :: Expression -> Maybe (Identifier, Expression)
extractUnification (Assignment name value)        = Just (name, value)
extractUnification (FieldAssignment _ name value) = Just (name, value)
extractUnification (Variable name value)          = Just (name, value)
extractUnification (Attribute name value)         = Just (name, value)
extractUnification _                              = Nothing

extractSubroutine :: Expression -> Maybe (Identifier, SubroutineBody)
extractSubroutine (Function name body)        = Just (name, body)
extractSubroutine (Procedure name body)       = Just (name, body)
extractSubroutine (Method name body)          = Just (name, body)
extractSubroutine (PrimitiveMethod op body)   = Just (show op, body)
extractSubroutine _                           = Nothing

extractParams :: Expression -> Maybe ([Pattern])
extractParams (Subroutine _ equations) = Just (equationParams.head $ equations)
extractParams (Clause _ params _)      = Just params
extractParams _                        = Nothing

extractCall :: Expression -> Maybe (Expression, [Expression])
extractCall (Application op args)   = Just (op, args)
extractCall (Send receptor op args) = Just (op, (receptor:args))
extractCall _                       = Nothing

extractClause :: Expression -> Maybe (Identifier, [Pattern], [Expression])
extractClause (Fact name patterns)             = Just (name, patterns, [])
extractClause (Rule name patterns expressions) = Just (name, patterns, expressions)
extractClause _                                = Nothing

extractReference :: Expression -> Maybe Identifier
extractReference (Reference n)        = Just n
extractReference (FieldReference _ n) = Just n
extractReference (Exist n _)          = Just n
extractReference _                    = Nothing
