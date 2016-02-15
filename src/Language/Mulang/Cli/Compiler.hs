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
compileInspection "declaresObject"        (Just o)  = declaresObject o
compileInspection "declaresAttribute"     (Just o)  = declaresAttribute o
compileInspection "declaresMethod"        (Just o)  = declaresMethod o
compileInspection "declaresFunction"      (Just o)  = declaresFunction o
compileInspection "declaresTypeAlias"     (Just o)  = declaresTypeAlias o
compileInspection "declaresTypeSignature" (Just o)  = declaresTypeSignature o
compileInspection "declares"              (Just o)  = declares o
compileInspection "declaresRecursively"   (Just o)  = declares o
compileInspection "declaresWithArity0"    (Just o)  = declaresWithArity 0 o
compileInspection "declaresWithArity1"    (Just o)  = declaresWithArity 1 o
compileInspection "declaresWithArity2"    (Just o)  = declaresWithArity 2 o
compileInspection "declaresWithArity3"    (Just o)  = declaresWithArity 3 o
compileInspection "declaresWithArity4"    (Just o)  = declaresWithArity 4 o
compileInspection "uses"                  (Just o)  = uses o
compileInspection "usesComposition"       Nothing   = usesComposition
compileInspection "usesGuards"            Nothing   = usesGuards
compileInspection "usesIf"                Nothing   = usesIf
compileInspection "usesWhile"             Nothing   = usesWhile
compileInspection "usesLambda"            Nothing   = usesLambda
compileInspection "usesComprehension"     Nothing   = usesComprehension
compileInspection "usesAnnonymousVariable"Nothing   = usesAnnonymousVariable

compileSubject :: [String] -> Bool -> (Inspection -> Inspection)
compileSubject s True          = (`transitiveList` s)
compileSubject s _             = (`scopedList` s)

