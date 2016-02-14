{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Compiler(
    compile,
    Expectation(..)) where

import GHC.Generics
import Data.Aeson
import Language.Mulang.Inspector.Combiner
import Language.Mulang.Inspector

data Expectation = Expectation {
  subject :: [String] ,
  verb :: String,
  object :: Maybe String,
  negated :: Bool,
  transitive :: Bool
} deriving (Show, Eq, Generic)

instance FromJSON Expectation
instance ToJSON Expectation


compile :: Expectation -> Inspection
compile (Expectation s v o n t) = compileSubject s t . compileNegation n $ compileInspection v o

compileNegation :: Bool -> Inspection -> Inspection
compileNegation False i = i
compileNegation _     i = negative i

compileInspection :: String -> Maybe String -> Inspection
compileInspection "declaresObject" (Just o) = declaresObject o

compileSubject :: [String] -> Bool -> (Inspection -> Inspection)
compileSubject s True          = (`transitiveList` s)
compileSubject s _             = (`scopedList` s)



