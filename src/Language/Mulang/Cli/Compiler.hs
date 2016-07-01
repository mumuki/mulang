{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Compiler(
    compile,
    Expectation(..),
    BindingPattern(..)) where

import GHC.Generics
import Data.Aeson
import Language.Mulang.Inspector
import Language.Mulang.Inspector.Combiner
import Language.Mulang.Binding (Binding, BindingPredicate)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)

data BindingPattern = Named String | Like String | Anyone deriving (Show, Eq, Generic)

data Expectation =  Advanced {
                      subject :: [String] ,
                      verb :: String,
                      object :: BindingPattern,
                      transitive :: Bool,
                      negated :: Bool
                    } 
                    | Basic {
                      binding :: String,
                      inspection :: String
                    } deriving (Show, Eq, Generic)

instance FromJSON BindingPattern
instance FromJSON Expectation

instance ToJSON BindingPattern
instance ToJSON Expectation

compile :: Expectation -> Inspection
compile (Advanced s v o t n) = compileSubject s t . compileNegation n $ compileInspection v (compilePattern o)
compile (Basic b i)          = (compile . toAdvanced b (withoutNegation splitedInspection)) isNegated
                               where splitedInspection = splitOn ":" i
                                     isNegated = elem "Not" splitedInspection

withoutNegation :: [String] -> [String]
withoutNegation = filter (/= "Not")

toAdvanced :: String -> [String] -> Bool -> Expectation
toAdvanced b ["HasAnonymousVariable"]      = nonTransitiveNamed b "usesAnonymousVariable"
toAdvanced b ["HasArity", n]               = nonTransitiveNamed b ("declaresWithArity" ++ n)
toAdvanced b ["HasBinding"]                = nonTransitiveNamed b "declares"
toAdvanced b ["HasGuards"]                 = nonTransitiveNamed b "usesGuards"
toAdvanced b ["HasIf"]                     = nonTransitiveNamed b "usesIf"
toAdvanced b ["HasTypeSignature"]          = nonTransitiveNamed b "declaresTypeSignature"
toAdvanced b ["HasTypeDeclaration"]        = nonTransitiveNamed b "declaresTypeAlias"
toAdvanced b ["HasUsage", x]               = Advanced [b] "uses" (Named x) True

nonTransitiveNamed binding inspection = Advanced [] inspection (Named binding) False

--"HasComposition",
--"HasComprehension",
--"HasDirectRecursion",
--"HasFindall",
--"HasForall",
--"HasForeach",
--"HasLambda",
--"HasNot",
--"HasPrefixApplication",
--"HasRepeat",
--"HasVariable",
--"HasWhile",
--"HasConditional",

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
compileInspection "usesRepeat"             _    = usesRepeat
compileInspection "usesWhile"              _    = usesWhile
compileInspection "usesLambda"             _    = usesLambda
compileInspection "usesComprehension"      _    = usesComprehension
compileInspection "usesAnonymousVariable"  _    = usesAnonymousVariable

compilePattern :: BindingPattern -> BindingPredicate
compilePattern (Named o) = named o
compilePattern (Like o)  = like o
compilePattern _         = anyone

compileSubject :: [String] -> Bool -> (Inspection -> Inspection)
compileSubject s True          = (`transitiveList` s)
compileSubject s _             = (`scopedList` s)

