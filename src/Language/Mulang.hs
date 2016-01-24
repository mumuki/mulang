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


data Program = Program [Declaration] deriving (Show)

type Identifier = String

data Declaration
         = TypeAlias Identifier
         | RecordDeclaration Identifier
         | TypeSignature Identifier
         | FunctionDeclaration Identifier [Equation]
         | ConstantDeclaration Identifier Rhs [Declaration]
  deriving (Eq,Show)

data Equation = Equation [Pattern] Rhs [Declaration] deriving (Eq,Show)

data Rhs
         = UnguardedRhs Expression
         | GuardedRhss  [GuardedRhs]
  deriving (Eq,Show)

data GuardedRhs = GuardedRhs Expression Expression deriving (Eq,Show)

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
  deriving (Eq,Show)

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
  deriving (Eq,Show)

data MuStmt
        = MuGenerator Pattern Expression
        | MuQualifier Expression
        | LetStmt [Declaration]
  deriving (Eq,Show)

data Alternative = Alternative Pattern GuardedAlternatives [Declaration] deriving (Eq,Show)

data GuardedAlternatives
        = UnguardedAlternative Expression          -- ^ @->@ /exp/
        | GuardedAlternatives  [GuardedAlternative] -- ^ /gdpat/
  deriving (Eq,Show)

data GuardedAlternative = GuardedAlternative Expression Expression deriving (Eq,Show)

data LiteralValue
          = MuBool Bool
          | MuInteger Integer
          | MuFloat Rational
          | MuString String
    deriving (Eq,Show)