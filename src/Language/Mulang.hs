{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang (
    Equation(..),
    EquationBody(..),
    Expression(..),
    ComprehensionStatement(..),
    Pattern(..),
    Identifier
  ) where

import           GHC.Generics

type Identifier = String

data Equation = Equation [Pattern] EquationBody deriving (Eq, Show, Read, Generic)

data EquationBody
         = UnguardedBody Expression
         | GuardedBody  [(Expression, Expression)]
  deriving (Eq, Show, Read, Generic)


-- expression or statement
-- may have effects
data Expression
        = TypeAliasDeclaration Identifier
        | RecordDeclaration Identifier
        | TypeSignature Identifier
        | FunctionDeclaration Identifier [Equation]    -- functional, maybe pure,
                                                        -- optionally guarded,
                                                        -- optionally pattern matched function
        | ProcedureDeclaration Identifier [Equation]    -- classic imperative-style procedure
        | MethodDeclaration Identifier [Equation]
        | VariableDeclaration Identifier Expression
        | VariableAssignment Identifier Expression
        | AttributeDeclaration Identifier Expression
        | ObjectDeclaration Identifier Expression
        | RuleDeclaration Identifier [Pattern] [Expression]
        | FactDeclaration Identifier [Pattern]
        | Exist Identifier [Pattern]
        | Not Expression
        | Forall Expression Expression
        | Variable Identifier
        | Application Expression [Expression]
        | Send Expression Expression [Expression]
        | Lambda [Pattern] Expression
        | If Expression Expression Expression
        | Return Expression
        | While Expression Expression
        | Repeat Expression Expression
        | Match Expression [Equation]
        | Comprehension Expression [ComprehensionStatement]
        | Sequence [Expression]
        | ExpressionOther
        | Equal
        | NotEqual
        | MuNull                                               -- nil, null, undefined or ()
        | MuObject Expression                                  -- literal, unnamed object
        | MuNumber Double                                      -- any number
        | MuBool Bool
        | MuString String
        | MuSymbol String
        | MuTuple [Expression]
        | MuList [Expression]
  deriving (Eq, Show, Read, Generic)

data Pattern
        = VariablePattern String                 -- ^ variable
        | LiteralPattern String              -- ^ literal constant
        | InfixApplicationPattern Pattern String Pattern
        | ApplicationPattern String [Pattern]        -- ^ data constructor and argument
        | TuplePattern [Pattern]              -- ^ tuple pattern
        | ListPattern [Pattern]               -- ^ list pattern
        | FunctorPattern String [Pattern]
        | AsPattern String Pattern         -- ^ @\@@-pattern
        | WildcardPattern                   -- ^ wildcard pattern (@_@)
        | OtherPattern
  deriving (Eq, Show, Read, Generic)

data ComprehensionStatement
        = MuGenerator Pattern Expression
        | MuQualifier Expression
        | LetStmt     Expression
  deriving (Eq, Show, Read, Generic)

