{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang (
    Equation(..),
    EquationBody(..),
    GuardedBody(..),
    Expression(..),
    ComprehensionStatement(..),
    Alternative(..),
    GuardedAlternatives(..),
    GuardedAlternative(..),
    Pattern(..)
  ) where

import           GHC.Generics

type Identifier = String

data Equation = Equation [Pattern] EquationBody deriving (Eq, Show, Read, Generic)

data EquationBody
         = UnguardedBody Expression
         | GuardedBodies  [GuardedBody]
  deriving (Eq, Show, Read, Generic)

data GuardedBody = GuardedBody Expression Expression deriving (Eq, Show, Read, Generic)

-- expression or statement
-- may have effects
data Expression
        = TypeAliasDeclaration Identifier
        | RecordDeclaration Identifier
        | TypeSignature Identifier
        | FunctionDeclaration Identifier [Equation]    -- functional, maybe pure,
                                                        -- optionally guarded,
                                                        -- optionally pattern matched function
        | ProcedureDeclaration Identifier              -- classic imperative-style procedure
        | VariableDeclaration Identifier Expression
        | Variable Identifier
        | InfixApplication Expression String Expression
        | Application Expression Expression
        | Send Expression Expression [Expression]
        | Lambda [Pattern] Expression
        | Let [Expression] Expression   -- TODO overlapping with variable RecordDeclaration
        | If Expression Expression Expression
        | While Expression Expression
        | Match Expression [Alternative]
        | Comprehension Expression [ComprehensionStatement]
        | Sequence [Expression]
        | ExpressionOther
        | MuUnit                                               -- nil, null, undefined or ()
        | MuObject Expression                                  -- literal, unnamed object
        | MuNumber Double                                      -- any number
        | MuBool Bool
        | MuString String
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
        | AsPattern String Pattern         -- ^ @\@@-pattern
        | WildcardPattern                   -- ^ wildcard pattern (@_@)
        | OtherPattern
  deriving (Eq, Show, Read, Generic)

data ComprehensionStatement
        = MuGenerator Pattern Expression
        | MuQualifier Expression
        | LetStmt     Expression
  deriving (Eq, Show, Read, Generic)


data Alternative = Alternative Pattern GuardedAlternatives deriving (Eq, Show, Read, Generic)

data GuardedAlternatives
        = UnguardedAlternative Expression          -- ^ @->@ /exp/
        | GuardedAlternatives  [GuardedAlternative] -- ^ /gdpat/
  deriving (Eq, Show, Read, Generic)


data GuardedAlternative = GuardedAlternative Expression Expression deriving (Eq, Show, Read, Generic)
