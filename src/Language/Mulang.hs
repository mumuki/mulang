{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang (
    Program(..),
    Declaration(..),
    Equation(..),
    Rhs(..),
    GuardedRhs(..),
    Expression(..),
    MuStmt(..),
    Alternative(..),
    GuardedAlternatives(..),
    GuardedAlternative(..),
    Pattern(..),
    LiteralValue(..)
  ) where

import           GHC.Generics
import           Data.Aeson

data Program = Program [Declaration] deriving (Eq, Show, Generic)

instance FromJSON Program
instance ToJSON Program

type Identifier = String

data Declaration
         = TypeAlias Identifier
         | RecordDeclaration Identifier
         | TypeSignature Identifier
         | FunctionDeclaration Identifier [Equation]
         | ConstantDeclaration Identifier Rhs [Declaration]
  deriving (Eq, Show, Generic)

instance FromJSON Declaration
instance ToJSON Declaration

data Equation = Equation [Pattern] Rhs [Declaration] deriving (Eq, Show, Generic)

instance FromJSON Equation
instance ToJSON Equation

data Rhs
         = UnguardedRhs Expression
         | GuardedRhss  [GuardedRhs]
  deriving (Eq, Show, Generic)

instance FromJSON Rhs
instance ToJSON Rhs


data GuardedRhs = GuardedRhs Expression Expression deriving (Eq, Show, Generic)

instance FromJSON GuardedRhs
instance ToJSON GuardedRhs

data Expression
        = Variable Identifier
        | Literal LiteralValue
        | InfixApplication Expression String Expression
        | Application Expression Expression
        | Lambda [Pattern] Expression
        | Let [Declaration] Expression          -- ^ local declarations with @let@
        | If Expression Expression Expression
        | Match Expression [Alternative]
        | MuTuple [Expression]
        | MuList [Expression]
        | ListComprehension Expression [MuStmt]
        | ExpressionOther
  deriving (Eq, Show, Generic)

instance FromJSON Expression
instance ToJSON Expression

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
  deriving (Eq, Show, Generic)

instance FromJSON Pattern
instance ToJSON Pattern

data MuStmt
        = MuGenerator Pattern Expression
        | MuQualifier Expression
        | LetStmt [Declaration]
  deriving (Eq, Show, Generic)

instance FromJSON MuStmt
instance ToJSON MuStmt

data Alternative = Alternative Pattern GuardedAlternatives [Declaration] deriving (Eq, Show,Generic)

instance FromJSON Alternative
instance ToJSON Alternative


data GuardedAlternatives
        = UnguardedAlternative Expression          -- ^ @->@ /exp/
        | GuardedAlternatives  [GuardedAlternative] -- ^ /gdpat/
  deriving (Eq, Show, Generic)

instance FromJSON GuardedAlternatives
instance ToJSON GuardedAlternatives

data GuardedAlternative = GuardedAlternative Expression Expression deriving (Eq, Show, Generic)

instance FromJSON GuardedAlternative
instance ToJSON GuardedAlternative

data LiteralValue
          = MuBool Bool
          | MuInteger Integer
          | MuFloat Rational
          | MuString String
    deriving (Eq, Show, Generic)


instance FromJSON LiteralValue
instance ToJSON LiteralValue
