{-# LANGUAGE DeriveGeneric, PatternSynonyms, ViewPatterns #-}

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
    subroutineBodyPatterns,
    equationPatterns,
    Code,
    Equation(..),
    EquationBody(..),
    Type(..),
    Modifier(..),
    Expression(..),
    Assertion(..),
    Statement(..),
    Pattern(..),
    Identifier,
    SubroutineBody,
    debug,
    debugType,
    debugModifier,
    debugPattern,
    other,
    otherType,
    otherModifier,
    otherPattern,
    pattern SimpleEquation,
    pattern SimpleFunction,
    pattern SimpleProcedure,
    pattern SimpleMethod,
    pattern SimpleSend,
    pattern PrimitiveSend,
    pattern SubroutineSignature,
    pattern VariableSignature,
    pattern ModuleSignature,
    pattern LValue,
    pattern LValuePattern,
    pattern Unification,
    pattern MuTrue,
    pattern MuFalse,
    pattern Subroutine,
    pattern Clause,
    pattern Call,
    pattern Params
  ) where

import           GHC.Generics
import           Language.Mulang.Identifier (Identifier)
import           Language.Mulang.Ast.Operator (Operator)

type Code = String

-- | An equation. See @Function@ and @Procedure@ above
data Equation = Equation [Pattern] EquationBody deriving (Eq, Show, Read, Generic, Ord)

data EquationBody
        = UnguardedBody Expression
        | GuardedBody  [(Expression, Expression)]
        deriving (Eq, Show, Read, Generic, Ord)

type SubroutineBody = [Equation]

-- A Generic Type, that can be used for typing expressions,
-- classes, functions, variables and so on, using a @TypeSignature@
-- or a @TypeCast@
data Type
        = SimpleType Identifier [Identifier]
        -- ^ simple types, with a type identifier and type constraints
        -- Useful for modelling variable types
        | ParameterizedType [Identifier] Identifier [Identifier]
        -- ^ parameterized types, with type inputs, type identifier and type constraints
        -- Useful for modelling functions, methods and procedures types
        | ConstrainedType [Identifier]
        -- ^ constrained type, with just type constraints.
        -- Usefull for modelling classes and interfaces types
        | OtherType (Maybe Code) (Maybe Type)
        -- ^ unrecognized type, with optional code and nested type
        deriving (Eq, Show, Read, Generic, Ord)

data Modifier
    = Abstract
    | Static
    | Private
    | Protected
    | Annotation Expression
    | OtherModifier (Maybe Code) (Maybe Modifier)
  deriving (Eq, Show, Read, Generic, Ord)

-- | Expression is the root element of a Mulang program.
-- | With the exception of Patterns, nearly everything is an Expression: variable declarations, literals,
-- | control structures, even object oriented classes declarations.
-- |
-- | However, although all those elements can be used as subexpressions and have an dohave an associated value,
-- | Mulang does not state WHICH is that value.
data Expression
    = TypeAlias Identifier Identifier
    -- ^ Functional programming type alias.
    --   Only the type alias identifier is parsed
    | Record Identifier
    -- ^ Imperative / Functional programming struct declaration.
    --   Only the record name is parsed
    | TypeSignature Identifier Type
    -- ^ Generic type signature for a computation,
    --   composed by a name and its type
    | TypeCast Expression Type
    -- ^ Generic type annotation for an expression. For example,
    -- a Java cast: (String) anObject => TypeAnnotation (Variable "anObject") "String"
    -- a Haskell inline type declaration: ... = x :: Int => TypeAnnotation (Variable "x") "Int"
    | EntryPoint Identifier Expression
    -- ^ Entry point with its body
    | Function Identifier SubroutineBody
    -- ^ Functional / Imperative programming function declaration.
    --   It is is composed by an identifier and one or more equations
    | Procedure Identifier SubroutineBody
    -- ^ Imperative programming procedure declaration. It is composed by a name and one or more equations
    | Method Identifier SubroutineBody
    | PrimitiveMethod Operator SubroutineBody
    | Variable Identifier Expression
    | Constant Identifier Expression
    | Assignment Identifier Expression
    | FieldAssignment Expression Identifier Expression
    -- ^ Generic nested field assignment
    | Attribute Identifier Expression
    -- ^ Object oriented programming attribute declaration, composed by an identifier and an initializer
    | Object Identifier Expression
    -- ^ Object oriented programming global, named object declaration,
    --   composed by a name and a body
    | Class Identifier (Maybe Identifier) Expression
    -- ^ Object oriented programming global, class declaration,
    --   composed by a name, an optional superclass, implemented interfaces and a body
    | EigenClass Expression Expression
    -- ^ Object oriented access to an object eigenclass, composed by the opened instance and the eigenclass body
    | Enumeration Identifier [Identifier]
    -- ^ Imperative named enumeration of values
    | Interface Identifier [Identifier] Expression
    -- ^ Object oriented programming global interface or contract declaration,
    --   composed by a name, superinterfaces and a body
    | Rule Identifier [Pattern] [Expression]
    -- ^ Logic programming declaration of a fact, composed by the rue name, rule arguments, and rule body
    | Fact Identifier [Pattern]
    -- ^ Logic programming declaration of a fact , composed by the fact name and fact arguments
    | Exist Identifier [Pattern]
    -- ^ Logic programming existential cuantification / consult
    | Not Expression
    -- ^ Logic programming negation
    | Findall Expression Expression Expression
    -- ^ Logic programming findall
    | Forall Expression Expression
    -- ^ Logic programming universal cuantification
    | Reference Identifier
    -- ^ Generic variable
    | FieldReference Expression Identifier
    -- ^ Generic nested field access
    | Primitive Operator
    -- ^ Reference to special, low level, universal operations like logical operaions and math, that may or may not be primitives
    -- in the original language
    | Application Expression [Expression]
    -- ^ Generic, non-curried application of a function or procedure, composed by the applied element itself, and the application arguments
    | Send Expression Expression [Expression]
    -- ^ Object oriented programming message send, composed by the reciever, selector and arguments
    | New Expression [Expression]
    -- ^ Object oriented instantiation, composed by the class reference and instantiation arguments
    | Implement Expression
    -- ^ Object oriented interface implementation
    | Include Expression
    -- ^ Object oriented mixin inclusion
    | Decorator [Modifier] Expression
    -- ^ Generic expression decorator for language modifiers and user-defined annotations
    -- ^ Eigenclass object
    | Lambda [Pattern] Expression
    | If Expression Expression Expression
    | Return Expression
    | Yield Expression
    | While Expression Expression
    -- ^ Imperative programming conditional repetition control structure, composed by a condition and a body
    | Repeat Expression Expression
    -- ^ Imperative programming fixed repetition control structure, composed by a repetition count expression, and a body
    | Match Expression [Equation]
    | Switch Expression [(Expression, Expression)] Expression
    -- ^ Generic switch expression, composed by a list of cases and a default case
    | Break Expression
    -- ^ Imperative programming break of flow statement
    | Continue Expression
    -- ^ Imperative programming step skip statement
    | Try Expression [(Pattern, Expression)] Expression
    -- ^ Generic try expression, composed by a body, a list of exception-handling patterns and statments, and a finally expression
    | Raise Expression
    -- ^ Generic raise expression, like a throw or raise statament, composed by the raised expression
    | Print Expression
    -- ^ Generic print expression
    | For [Statement] Expression
    -- ^ For-comprehension / For-each statement
    | ForLoop Expression Expression Expression Expression
    -- ^ Imperative / OOP programming c-style for loop
    | Sequence [Expression]
    -- ^ Generic sequence of statements
    | Other (Maybe Code) (Maybe Expression)
    -- ^ Unrecognized expression, with optional description and body
    | Arrow Expression Expression
    -- ^ Generic arrow - AKA pair or entry - that is typically used to build dictionaries.
    -- It corresponds to ruby's, perl's and php's `=>` operator, or ruby's and javascript's `:` operator
    | Self
    -- ^ Object oriented self-refence literal - AKA this or me
    | None
    -- ^ Generic value indicating an absent expression,
    -- such as when there is no finally in a try or default in a switch or js' undefined
    | MuNil
    -- ^ Generic nothing value literal - AKA nil, null or unit
    | MuDict Expression
    -- ^ Generic dictionary - AKA hash, table or map - value literal.
    -- Its expressions are normally a sequence of Arrows
    | MuObject Expression
    -- ^ Object oriented unnamed object literal
    | MuNumber Double
    -- ^ Generic number literal
    | MuBool Bool
    -- ^ Generic boolean literal
    | MuString String
    -- ^ Generic string literal
    | MuChar Char
    -- ^ Generic char literal
    | MuSymbol String
    -- ^ Generic symbol/atom literal
    | MuTuple [Expression]
    -- ^ Generic tuple value literal
    | MuList [Expression]
    -- ^ Generic list or stream value literal
    | TestGroup Expression Expression
    -- ^ Generic test grouping expression such as describe, context, etc.
    | Test Expression Expression
    -- ^ Generic test expression such as it, etc.
    | Assert Bool Assertion
    -- ^ Generic assertion expression such as assert, expect, etc. The first parameter indicates whether the assertion is negated or not
  deriving (Eq, Show, Read, Generic, Ord)

data Assertion
    = Equality Expression Expression
    -- ^ assert equality between two expressions. e.g.: assert.equals(succ(3), 4)
    | Truth Expression
    -- ^ assert truthfulness of boolean expression. e.g.: assert(4 > 3)
    | Failure Expression Expression
    -- ^ assert expression fails with an error. e.g.: assert.throws(function () { throw("this breaks") }, "this breaks")
  deriving (Eq, Show, Read, Generic, Ord)

-- | Mulang Patterns are not expressions, but are aimed to match them.
-- | They are modeled after Haskell, Prolog, Elixir, Scala and Erlang patterns
data Pattern
    = VariablePattern Identifier
    -- ^ variable pattern like @X@
    | ConstantPattern Identifier
    -- ^ constant pattern like @X@
    | LiteralPattern Code
    -- ^ literal constant pattern like @4@
    | InfixApplicationPattern Pattern Identifier Pattern
    -- ^ infix application pattern like @4:X@
    | ApplicationPattern Identifier [Pattern]
    -- ^ prefix application pattern like @f _@
    | TuplePattern [Pattern]
    -- ^ tuple pattern like @(3, _)@
    | ListPattern [Pattern]
    -- ^ list pattern like @[x, y, _]@
    | FunctorPattern Identifier [Pattern]
    -- ^ prolog-like functor pattern, like @f(X, 6)@
    | AsPattern Identifier Pattern
    | TypePattern Identifier
    -- ^ @\@@-pattern
    | WildcardPattern
    -- ^ wildcard pattern @_@
    | UnionPattern [Pattern]
    | OtherPattern (Maybe Code) (Maybe Pattern)
    -- ^ Other unrecognized pattern with optional code and nested pattern
  deriving (Eq, Show, Read, Generic, Ord)

data Statement
  = Generator Pattern Expression
  | Guard Expression
  deriving (Eq, Show, Read, Generic, Ord)

debug :: Show a => a -> Expression
debug a = Other (Just (show a)) Nothing

debugType :: Show a => a -> Type
debugType a = OtherType (Just (show a)) Nothing

debugModifier :: Show a => a -> Modifier
debugModifier a = OtherModifier (Just (show a)) Nothing

debugPattern :: Show a => a -> Pattern
debugPattern a = OtherPattern (Just (show a)) Nothing

other :: String -> Expression -> Expression
other s e = Other (Just s) (Just e)

otherType :: String -> Type -> Type
otherType s e = OtherType (Just s) (Just e)

otherModifier :: String -> Modifier -> Modifier
otherModifier s e = OtherModifier (Just s) (Just e)

otherPattern :: String -> Pattern -> Pattern
otherPattern s e = OtherPattern (Just s) (Just e)

pattern VariableSignature name t cs        = TypeSignature name (SimpleType t cs)
pattern SubroutineSignature name args t cs = TypeSignature name (ParameterizedType args t cs)
pattern ModuleSignature name cs            = TypeSignature name (ConstrainedType cs)

pattern SimpleEquation params body = Equation params (UnguardedBody body)

pattern SimpleSend receptor selector args = Send receptor (Reference selector) args
pattern PrimitiveSend receptor selector args = Send receptor (Primitive selector) args

pattern SimpleFunction name params body  = Function  name [SimpleEquation params body]
pattern SimpleProcedure name params body = Procedure name [SimpleEquation params body]
pattern SimpleMethod name params body    = Method    name [SimpleEquation params body]

pattern LValue name value <- (extractLValue -> Just (name, value))

pattern LValuePattern name <- (extractLValuePattern -> Just name)

pattern Unification name value <- (extractUnification -> Just (name, value))

pattern MuTrue  = MuBool True
pattern MuFalse = MuBool False

pattern Subroutine name body <- (extractSubroutine -> Just (name, body))
pattern Clause name patterns expressions <- (extractClause -> Just (name, patterns, expressions))

pattern Call operation arguments <- (extractCall -> Just (operation, arguments))

pattern Params params <- (extractParams -> Just params)

equationParams :: Equation -> [Pattern]
equationParams (Equation p _) = p

subroutineBodyPatterns :: SubroutineBody -> [Pattern]
subroutineBodyPatterns = concatMap equationPatterns

equationPatterns :: Equation -> [Pattern]
equationPatterns (Equation p _) = p

extractLValue :: Expression -> Maybe (Identifier, Expression)
extractLValue (Variable name value) = Just (name, value)
extractLValue (Constant name value) = Just (name, value)
extractLValue _                     = Nothing

extractLValuePattern :: Pattern -> Maybe Identifier
extractLValuePattern (VariablePattern name) = Just name
extractLValuePattern (ConstantPattern name) = Just name
extractLValuePattern _                      = Nothing

extractUnification :: Expression -> Maybe (Identifier, Expression)
extractUnification (Assignment name value)        = Just (name, value)
extractUnification (FieldAssignment _ name value) = Just (name, value)
extractUnification (LValue name value)            = Just (name, value)
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
