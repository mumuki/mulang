{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Compiler(
    compile,
    Expectation(..)) where

import GHC.Generics
import Data.Aeson
import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner
import Language.Mulang.Explorer (Binding, BindingPredicate)

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
compile (Expectation s v o n t) = compileSubject s t . compileNegation n $ compileInspection v (compilePattern o)

compileNegation :: Bool -> Inspection -> Inspection
compileNegation False i = i
compileNegation _     i = negative i

compileInspection :: String -> BindingPredicate -> Inspection
compileInspection "declaresObject"         pred = declaresObject pred
compileInspection "declaresAttribute"      pred = declaresAttribute pred
compileInspection "declaresMethod"         pred = declaresMethod pred
compileInspection "declaresFunction"       pred = declaresFunction pred
compileInspection "declaresTypeAlias"      pred = declaresTypeAlias pred
compileInspection "declaresTypeSignature"  pred = declaresTypeSignature pred
compileInspection "declares"               pred = declares pred
compileInspection "declaresRecursively"    pred = declares pred
compileInspection "declaresWithArity0"     pred = declaresWithArity 0 pred
compileInspection "declaresWithArity1"     pred = declaresWithArity 1 pred
compileInspection "declaresWithArity2"     pred = declaresWithArity 2 pred
compileInspection "declaresWithArity3"     pred = declaresWithArity 3 pred
compileInspection "declaresWithArity4"     pred = declaresWithArity 4 pred
compileInspection "uses"                   pred = uses pred
compileInspection "usesComposition"        _    = usesComposition
compileInspection "usesGuards"             _    = usesGuards
compileInspection "usesIf"                 _    = usesIf
compileInspection "usesWhile"              _    = usesWhile
compileInspection "usesLambda"             _    = usesLambda
compileInspection "usesComprehension"      _    = usesComprehension
compileInspection "usesAnnonymousVariable" _    = usesAnnonymousVariable

compilePattern :: (Maybe Binding) -> BindingPredicate
compilePattern (Just o) = (== o)

compileSubject :: [String] -> Bool -> (Inspection -> Inspection)
compileSubject s True          = (`transitiveList` s)
compileSubject s _             = (`scopedList` s)

