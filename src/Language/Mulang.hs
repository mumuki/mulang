{-# LANGUAGE DeriveGeneric #-}

-- | The Mulang module describes a generic, abstract language AST. 
-- |
-- | Since it only describes the AST, but not many aspects of its semantics, Mulang is non-computable, but just a target
-- | for code analysys. 
-- |
-- | Mulang AST itself is the combination of other abstract, paradigmatic languages that express the structures of: 
-- |    * imperative programing
-- |    * object-oriented programing
-- |    * functional programing
-- |    * logic programing
-- |
module Language.Mulang (
    Equation(..),
    EquationBody(..),
    Expression(..),
    ComprehensionStatement(..),
    Pattern(..),
    Identifier
  ) where

import           GHC.Generics

-- | An identifier
-- | Mulang does not assume any special naming convention or format
type Identifier = String

-- | An equation. See @FunctionDeclaration@ and @ProcedureDeclaration@ above
data Equation = Equation [Pattern] EquationBody deriving (Eq, Show, Read, Generic)

data EquationBody
         = UnguardedBody Expression
         | GuardedBody  [(Expression, Expression)]
  deriving (Eq, Show, Read, Generic)


-- | Expression is the root element of a Mulang program. 
-- | With the exception of Patterns, nearly everything is an Expression: variable declarations, literals, 
-- | constrol structures, even object oriented classes declarations.
-- |
-- | However, although all those elements can be used as subexpressions and have an dohave an associated value, 
-- | Mulang does not state WHICH is that value. 
data Expression
        = TypeAliasDeclaration Identifier                    -- ^ Functional programming type alias. Only the type alias identifier is parsed 
        | RecordDeclaration Identifier                       -- ^ Imperative / Functional programming struct declaration. Only the record name is parsed
        | TypeSignature Identifier                           -- ^ Generic type signature for a computation. Only the target name of the computation is parsed
        | FunctionDeclaration Identifier [Equation]          -- ^ Functional / Imperative programming function declaration. It is is composed by an identifier and one or more equations
        | ProcedureDeclaration Identifier [Equation]         -- ^ Imperative programming procedure declaration. It is composed by a name and one or more equations
        | MethodDeclaration Identifier [Equation] 
        | VariableDeclaration Identifier Expression
        | VariableAssignment Identifier Expression
        | AttributeDeclaration Identifier Expression         -- ^ Object oriented programming attribute declaration, composed by an identifier and an initializer
        | ObjectDeclaration Identifier Expression            -- ^ Object oriented programming named object declaration, composed by a name and a body
        | RuleDeclaration Identifier [Pattern] [Expression]  -- ^ Logic programming declaration of a fact, composed by the rue name, rule arguments, and rule body 
        | FactDeclaration Identifier [Pattern]               -- ^ Logic programming declaration of a fact , composed by the fact name and fact arguments
        | Exist Identifier [Pattern]                         -- ^ Logic programming existential cuantification / consult 
        | Not Expression                                     -- ^ Logic programming negation 
        | Findall Expression Expression Expression           -- ^ Logic programming findall 
        | Forall Expression Expression                       -- ^ Logic programming universal cuantification
        | Variable Identifier                                -- ^ Generic variable 
        | Application Expression [Expression]                -- ^ Generic, non-curried application of a function or procedure, composed by the applied element itself, and the application arguments
        | Send Expression Expression [Expression]            -- ^ Object oriented programming message send, composed by the reciever, selector and arguments
        | Lambda [Pattern] Expression
        | If Expression Expression Expression
        | Return Expression
        | While Expression Expression                         -- ^ Imperative programming conditional repetition control structure, composed by a condition and a body 
        | Repeat Expression Expression                        -- ^ Imperative programming fixed repetition control structure, composed by a repetition count expression, and a body
        | Match Expression [Equation]
        | Switch Expression [(Expression,Expression)]
        | Comprehension Expression [ComprehensionStatement]
        | Sequence [Expression]                                -- ^ Generic sequence of statements
        | ExpressionOther
        | Equal
        | NotEqual
        | MuNull                                               -- ^ Generic nothing value literal - nil, null, undefined or unit
        | MuObject Expression                                  -- ^ Object oriented unnamed object literal
        | MuNumber Double                                      -- ^ Generic number literal
        | MuBool Bool                                          -- ^ Generic boolean literal
        | MuString String                                      -- ^ Generic string literal
        | MuSymbol String                                      -- ^ Generic symbol/atom literal 
        | MuTuple [Expression]
        | MuList [Expression]
  deriving (Eq, Show, Read, Generic)

-- | Mulang Patterns are not expressions, but are aimed to match them.
-- | They are modeled after Haskell, Prolog, Elixir, Scala and Erlang patterns
data Pattern
        = VariablePattern String                               -- ^ variable pattern like @X@
        | LiteralPattern String                                -- ^ literal constant pattern like @4@     
        | InfixApplicationPattern Pattern String Pattern       -- ^ infix application pattern like @4:X@
        | ApplicationPattern String [Pattern]                  -- ^ prefix application pattern like @f _@
        | TuplePattern [Pattern]                               -- ^ tuple pattern like @(3, _)@
        | ListPattern [Pattern]                                -- ^ list pattern like @[x, y, _]@
        | FunctorPattern String [Pattern]                      -- ^ prolog-like functor pattern, like @f(X, 6)@
        | AsPattern String Pattern                             -- ^ @\@@-pattern
        | WildcardPattern                                      -- ^ wildcard pattern @_@
        | OtherPattern                                         -- ^ Other unrecognized pattern
  deriving (Eq, Show, Read, Generic)

data ComprehensionStatement
        = MuGenerator Pattern Expression
        | MuQualifier Expression
        | LetStmt     Expression
  deriving (Eq, Show, Read, Generic)

