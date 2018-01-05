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
    Expression(..),
    Statement(..),
    Pattern(..),
    Identifier,
    SubroutineBody,
    debug,
    pattern Other,
    pattern OtherBody,
    pattern SimpleEquation,
    pattern SimpleFunction,
    pattern SimpleProcedure,
    pattern SimpleMethod,
    pattern SimpleSend,
    pattern MuTrue,
    pattern MuFalse,
    pattern SubroutineTypeSignature,
    pattern Subroutine,
    pattern Clause,
    pattern Call,
    pattern Params
  ) where

import           GHC.Generics
import           Language.Mulang.Identifier (Identifier)

type Code = String

-- | An equation. See @Function@ and @Procedure@ above
data Equation = Equation [Pattern] EquationBody deriving (Eq, Show, Read, Generic)

data EquationBody
         = UnguardedBody Expression
         | GuardedBody  [(Expression, Expression)]
  deriving (Eq, Show, Read, Generic)

type SubroutineBody = [Equation]

-- | Expression is the root element of a Mulang program.
-- | With the exception of Patterns, nearly everything is an Expression: variable declarations, literals,
-- | control structures, even object oriented classes declarations.
-- |
-- | However, although all those elements can be used as subexpressions and have an dohave an associated value,
-- | Mulang does not state WHICH is that value.
data Expression
    = TypeAlias Identifier
    -- ^ Functional programming type alias.
    --   Only the type alias identifier is parsed
    | Record Identifier
    -- ^ Imperative / Functional programming struct declaration.
    --   Only the record name is parsed
    | TypeSignature Identifier (Maybe [Identifier]) Identifier
    -- ^ Generic type signature for a computation,
    --   composed by a name, parameter types and return type
    | EntryPoint Identifier Expression
    -- ^ Entry point with its body
    | Function Identifier SubroutineBody
    -- ^ Functional / Imperative programming function declaration.
    --   It is is composed by an identifier and one or more equations
    | Procedure Identifier SubroutineBody
    -- ^ Imperative programming procedure declaration. It is composed by a name and one or more equations
    | Method Identifier SubroutineBody
    | EqualMethod SubroutineBody
    | HashMethod SubroutineBody
    | Variable Identifier Expression
    | Assignment Identifier Expression
    | Attribute Identifier Expression
    -- ^ Object oriented programming attribute declaration, composed by an identifier and an initializer
    | Object Identifier Expression
    -- ^ Object oriented programming global, named object declaration,
    --   composed by a name and a body
    | Class Identifier (Maybe Identifier) Expression
    -- ^ Object oriented programming global, class declaration,
    --   composed by a name, an optional superclass, implemented interfaces and a body
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
    | Application Expression [Expression]
    -- ^ Generic, non-curried application of a function or procedure, composed by the applied element itself, and the application arguments
    | Send Expression Expression [Expression]
    -- ^ Object oriented programming message send, composed by the reciever, selector and arguments
    | New Identifier [Expression]
    -- ^ Object oriented instantiation, composed by the class reference and instantiation arguments
    | Implement Identifier
    -- ^ Object oriented instantiation, interface implementation
    | Include Identifier
    -- ^ Object oriented instantiation, mixin inclusion
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
    | Try Expression [(Pattern, Expression)] Expression
    -- ^ Generic try expression, composed by a body, a list of exception-handling patterns and statments, and a finally expression
    | Raise Expression
    -- ^ Generic raise expression, like a throw or raise statament, composed by the raised expression
    | Print Expression
    -- ^ Generic print expression
    | For [Statement] Expression
    | Sequence [Expression]
    -- ^ Generic sequence of statements
    | Unknown (Maybe String) (Maybe Expression)
    -- ^ Unrecognized expression, with optional description and body
    | Equal
    | NotEqual
    | Self
    | MuNull
    -- ^ Generic value indicating an absent expression, such as when there is no finally in a try or default in a switch or js' undefined
    | MuNil
    -- ^ Generic nothing value literal - nil, null or unit
    | MuObject Expression
    -- ^ Object oriented unnamed object literal
    | MuNumber Double
    -- ^ Generic number literal
    | MuBool Bool
    -- ^ Generic boolean literal
    | MuString String
    -- ^ Generic string literal
    | MuSymbol String
    -- ^ Generic symbol/atom literal
    | MuTuple [Expression]
    | MuList [Expression]
  deriving (Eq, Show, Read, Generic)

-- | Mulang Patterns are not expressions, but are aimed to match them.
-- | They are modeled after Haskell, Prolog, Elixir, Scala and Erlang patterns
data Pattern
    = VariablePattern String
    -- ^ variable pattern like @X@
    | LiteralPattern String
    -- ^ literal constant pattern like @4@
    | InfixApplicationPattern Pattern String Pattern
    -- ^ infix application pattern like @4:X@
    | ApplicationPattern String [Pattern]
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
    | OtherPattern
    -- ^ Other unrecognized pattern
  deriving (Eq, Show, Read, Generic)

data Statement
  = Generator Pattern Expression
  | Guard Expression
  deriving (Eq, Show, Read, Generic)

debug :: Show a => a -> Expression
debug a = Unknown (Just (show a)) Nothing

pattern Other = Unknown Nothing Nothing
pattern OtherBody body <- Unknown _ (Just body)

pattern SimpleEquation params body = Equation params (UnguardedBody body)

pattern SimpleSend receptor selector args = Send receptor (Reference selector) args

pattern SimpleFunction name params body  = Function  name [SimpleEquation params body]
pattern SimpleProcedure name params body = Procedure name [SimpleEquation params body]
pattern SimpleMethod name params body    = Method    name [SimpleEquation params body]

pattern MuTrue  = MuBool True
pattern MuFalse = MuBool False

pattern SubroutineTypeSignature name params return = TypeSignature name (Just params) return

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

extractSubroutine :: Expression -> Maybe (Identifier, SubroutineBody)
extractSubroutine (Function name body)  = Just (name, body)
extractSubroutine (Procedure name body) = Just (name, body)
extractSubroutine (Method name body)    = Just (name, body)
extractSubroutine _                     = Nothing

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
