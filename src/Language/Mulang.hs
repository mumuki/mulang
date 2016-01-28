{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang (
    Declaration(..),
    Equation(..),
    Rhs(..),
    GuardedRhs(..),
    Expression(..),
    ComprehensionStatement(..),
    Alternative(..),
    GuardedAlternatives(..),
    GuardedAlternative(..),
    Pattern(..)
  ) where

import           GHC.Generics

type Identifier = String

-- declaration or directive
data Declaration
         = TypeAlias Identifier
         | RecordDeclaration Identifier
         | TypeSignature Identifier
         | FunctionDeclaration Identifier [Equation]    -- functional, maybe pure,
                                                        -- optionally guarded,
                                                        -- optionally pattern matched function
         | ProcedureDeclaration Identifier              -- classic imperative-style procedure
         | VariableDeclaration Identifier Expression
  deriving (Eq, Show, Read, Generic)

data Equation = Equation [Pattern] Rhs deriving (Eq, Show, Read, Generic)

data Rhs
         = UnguardedRhs Expression
         | GuardedRhss  [GuardedRhs]
  deriving (Eq, Show, Read, Generic)

data GuardedRhs = GuardedRhs Expression Expression deriving (Eq, Show, Read, Generic)

-- expression or statement
-- may have effects
data Expression
        = DeclarationExpression Declaration
        | Variable Identifier
        | InfixApplication Expression String Expression
        | Application Expression Expression
        | Lambda [Pattern] Expression
        | Let [Declaration] Expression
        | If Expression Expression Expression
        | While Expression Expression
        | Match Expression [Alternative]
        | Comprehension Expression [ComprehensionStatement]
        | Sequence [Expression]
        | ExpressionOther
        | MuObject Expression
        | MuNumber Double
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
