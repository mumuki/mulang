{-# LANGUAGE DeriveGeneric #-}

module Language.Mulang.Cli.Compiler(
  compile,
  Expectation(..),
  BindingPattern(..)) where

import GHC.Generics
import Data.Aeson
import Language.Mulang
import Data.Maybe (fromMaybe)
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
compile = fromMaybe (\_ -> True) . compileMaybe

compileMaybe :: Expectation -> Maybe Inspection
compileMaybe (Advanced s v o t n) = do
                                inspection <- compileInspection v (compilePattern o)
                                return . compileSubject s t . compileNegation n $ inspection
compileMaybe (Basic b i) = do
                        let splitedInspection = splitOn ":" i
                        let isNegated = elem "Not" splitedInspection
                        expectation <- toAdvanced b (withoutNegation splitedInspection) isNegated
                        return $ compile expectation


withoutNegation :: [String] -> [String]
withoutNegation = filter (/= "Not")

toAdvanced :: String -> [String] -> Bool -> Maybe Expectation
toAdvanced b ["HasAnonymousVariable"] = Just . nonTransitiveAdv b "usesAnonymousVariable"
toAdvanced b ["HasArity", n]          = Just . nonTransitiveAdv b ("declaresComputationWithArity" ++ n)
toAdvanced b ["HasBinding"]           = Just . Advanced [] "declares" (Named b) False
toAdvanced b ["HasComposition"]       = Just . transitiveAdv b "usesComposition"
toAdvanced b ["HasComprehension"]     = Just . transitiveAdv b "usesComprehension"
toAdvanced b ["HasConditional"]       = Just . transitiveAdv b "usesConditional"
toAdvanced b ["HasDirectRecursion"]   = Just . Advanced [] "declaresRecursively" (Named b) False
toAdvanced b ["HasFindall"]           = Just . transitiveAdv b "usesFindall"
toAdvanced b ["HasForall"]            = Just . transitiveAdv b "usesForall"
toAdvanced b ["HasGuards"]            = Just . transitiveAdv b "usesGuards"
toAdvanced b ["HasIf"]                = Just . nonTransitiveAdv b "usesIf"
toAdvanced b ["HasLambda"]            = Just . transitiveAdv b "usesLambda"
toAdvanced b ["HasNot"]               = Just . nonTransitiveAdv b "usesNot"
toAdvanced b ["HasRepeat"]            = Just . nonTransitiveAdv b "usesRepeat"
toAdvanced b ["HasTypeSignature"]     = Just . Advanced [] "declaresTypeSignature" (Named b) False
toAdvanced b ["HasTypeDeclaration"]   = Just . nonTransitiveAdv b "declaresTypeAlias"
toAdvanced b ["HasUsage", x]          = Just . Advanced [b] "uses" (Named x) True
toAdvanced b ["HasWhile"]             = Just . nonTransitiveAdv b "usesWhile"
toAdvanced _ _                        = const Nothing
-- TODO:
--"HasForeach",
--"HasPrefixApplication",
--"HasVariable"

transitiveAdv     = adv True
nonTransitiveAdv  = adv False
adv negated binding inspection = Advanced [binding] inspection Anyone negated

compileNegation :: Bool -> Inspection -> Inspection
compileNegation False i = i
compileNegation _     i = negative i

compileInspection :: String -> BindingPredicate -> Maybe Inspection
compileInspection "declaresObject"                 pred = Just $ declaresObject pred
compileInspection "declaresAttribute"              pred = Just $ declaresAttribute pred
compileInspection "declaresMethod"                 pred = Just $ declaresMethod pred
compileInspection "declaresFunction"               pred = Just $ declaresFunction pred
compileInspection "declaresTypeAlias"              pred = Just $ declaresTypeAlias pred
compileInspection "declaresTypeSignature"          pred = Just $ declaresTypeSignature pred
compileInspection "declares"                       pred = Just $ declares pred
compileInspection "declaresRecursively"            pred = Just $ declaresRecursively pred
compileInspection "declaresComputation"            pred = Just $ declaresComputation pred
compileInspection "declaresComputationWithArity0"  pred = Just $ declaresComputationWithExactArity 0 pred
compileInspection "declaresComputationWithArity1"  pred = Just $ declaresComputationWithExactArity 1 pred
compileInspection "declaresComputationWithArity2"  pred = Just $ declaresComputationWithExactArity 2 pred
compileInspection "declaresComputationWithArity3"  pred = Just $ declaresComputationWithExactArity 3 pred
compileInspection "declaresComputationWithArity4"  pred = Just $ declaresComputationWithExactArity 4 pred
compileInspection "uses"                           pred = Just $ uses pred
compileInspection "usesAnonymousVariable"          _    = Just $ usesAnonymousVariable
compileInspection "usesComposition"                _    = Just $ usesComposition
compileInspection "usesComprehension"              _    = Just $ usesComprehension
compileInspection "usesConditional"                _    = Just $ usesConditional
compileInspection "usesGuards"                     _    = Just $ usesGuards
compileInspection "usesFindall"                    _    = Just $ usesFindall
compileInspection "usesForall"                     _    = Just $ usesForall
compileInspection "usesIf"                         _    = Just $ usesIf
compileInspection "usesLambda"                     _    = Just $ usesLambda
compileInspection "usesNot"                        _    = Just $ usesNot
compileInspection "usesRepeat"                     _    = Just $ usesRepeat
compileInspection "usesWhile"                      _    = Just $ usesWhile
compileInspection _                                _    = Nothing


compilePattern :: BindingPattern -> BindingPredicate
compilePattern (Named o) = named o
compilePattern (Like o)  = like o
compilePattern _         = anyone

compileSubject :: [String] -> Bool -> (Inspection -> Inspection)
compileSubject s True          = (`transitiveList` s)
compileSubject s _             = (`scopedList` s)

