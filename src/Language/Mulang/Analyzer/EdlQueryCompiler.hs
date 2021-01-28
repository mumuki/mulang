{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.EdlQueryCompiler(
  compileTopQuery,
  compileTopQuery') where

import Data.Char (toUpper)
import Data.Count (encode)
import Data.Function.Extra (orElse, andAlso, never)

import Language.Mulang
import Language.Mulang.Consult (Consult)
import Language.Mulang.Counter (plus)
import Language.Mulang.Inspector.Primitive (atLeast, atMost, exactly)
import Language.Mulang.Inspector.Literal (isNil, isNumber, isBool, isChar, isString, isSymbol, isSelf, isLiteral)
import Language.Mulang.Analyzer.Synthesizer (decodeIsInspection, decodeUsageInspection, decodeDeclarationInspection)

import qualified Language.Mulang.Edl.Expectation as E

import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

import           Data.Map (Map)
import qualified Data.Map.Strict as Map

type Scope = (ContextualizedInspection -> ContextualizedInspection, IdentifierPredicate -> IdentifierPredicate)
type AutocorrectionRules = Map E.CQuery E.CQuery

compileTopQuery :: E.Query -> Inspection
compileTopQuery = compileTopQuery' Map.empty

compileTopQuery' :: AutocorrectionRules -> E.Query -> Inspection
compileTopQuery' rules = fromMaybe (const True) . compileQuery rules

compileQuery :: Map E.CQuery E.CQuery -> E.Query -> Maybe Inspection
compileQuery rules (E.Decontextualize query) = compileCQuery rules id query >>= (return . decontextualize)
compileQuery rules (E.Within name query)     | (scope, p) <- compileWithin name  = fmap (decontextualize.scope) (compileCQuery rules p query)
compileQuery rules (E.Through name query)    | (scope, p) <- compileThrough name = fmap (decontextualize.scope) (compileCQuery rules p query)
compileQuery rules (E.Not query)             = fmap never (compileQuery rules query)
compileQuery rules (E.And query1 query2)     = andAlso <$> (compileQuery rules query1) <*> (compileQuery rules query2)
compileQuery rules (E.Or query1 query2)      = orElse <$> (compileQuery rules query1) <*> (compileQuery rules query2)

compileWithin, compileThrough :: String -> Scope
compileWithin name  = scopeFor scopedList name
compileThrough name = scopeFor transitiveList name

scopeFor :: ([Identifier] -> Inspection -> Inspection) -> Identifier -> Scope
scopeFor f name = (contextualized (f names), andAlso (except (last names)))
  where names = splitOn "." name

compileCQuery :: AutocorrectionRules -> (IdentifierPredicate -> IdentifierPredicate) -> E.CQuery -> Maybe ContextualizedInspection
compileCQuery rules pm (E.Inspection i p m) = compileOrCorrectInspection rules pm (E.Inspection (compileVerb i) p m)
compileCQuery rules pm (E.AtLeast n q)      = contextualized (atLeast (encode n)) <$> compileTQuery rules pm q
compileCQuery rules pm (E.AtMost n q)       = contextualized (atMost (encode n))  <$> compileTQuery rules pm q
compileCQuery rules pm (E.Exactly n q)      = contextualized (exactly (encode n)) <$> compileTQuery rules pm q
compileCQuery rules pm (E.CNot q)           = contextualized never                <$> compileCQuery rules pm q
compileCQuery rules pm (E.CAnd q1 q2)       = contextualized2 andAlso             <$> compileCQuery rules pm q1 <*> compileCQuery rules pm q2
compileCQuery rules pm (E.COr q1 q2)        = contextualized2 orElse              <$> compileCQuery rules pm q1 <*> compileCQuery rules pm q2

compileOrCorrectInspection rules pm query@(E.Inspection "Uses" _ _) = foo rules pm . fromMaybe query . Map.lookup query $ rules
compileOrCorrectInspection rules pm query@(E.Inspection "Declares" _ _) = foo rules pm . fromMaybe query . Map.lookup query $ rules
compileOrCorrectInspection rules pm query = foo rules pm query
foo rules pm (E.Inspection i p m) = ($ (compilePredicate pm p)) <$> compileInspection rules i m

compileTQuery :: AutocorrectionRules -> (IdentifierPredicate -> IdentifierPredicate) -> E.TQuery -> Maybe ContextualizedCounter
compileTQuery rules pm (E.Counter i p m) = compileOrCorrectCounter rules pm (E.Counter (compileVerb i) p m)
compileTQuery rules pm (E.Plus q1 q2)    = contextualized2 plus        <$> (compileTQuery rules pm q1) <*> (compileTQuery rules pm q2)

compileOrCorrectCounter rules pm query@(E.Counter "Uses" _ _) = foo1 rules pm . fromMaybe query . fmap toCounter . Map.lookup (fromCounter query) $ rules
compileOrCorrectCounter rules pm query = foo1 rules pm query
foo1 rules pm (E.Counter i p m) = ($ (compilePredicate pm p)) <$> compileCounter rules i m
toCounter (E.Inspection i p m) = (E.Counter i p m)
fromCounter (E.Counter i p m) = (E.Inspection i p m)

compilePredicate :: (IdentifierPredicate -> IdentifierPredicate) -> E.Predicate -> IdentifierPredicate
compilePredicate p E.Any             = p $ anyone
compilePredicate _ (E.Named name)    = named name
compilePredicate p (E.Except name)   = p $ except name
compilePredicate p (E.Like name)     = p $ like name
compilePredicate p (E.Unlike name)   = p $ unlike name
compilePredicate _ (E.AnyOf ns)      = anyOf ns
compilePredicate p (E.NoneOf ns)     = p $ noneOf ns
compilePredicate p (E.LikeAnyOf ns)  = p $ likeAnyOf ns
compilePredicate p (E.LikeNoneOf ns) = p $ likeNoneOf ns


compileVerb :: String -> String
compileVerb = concat . map headToUpper . words
  where headToUpper (x:xs) = toUpper x : xs

compileCounter :: AutocorrectionRules -> String -> E.Matcher -> Maybe (ContextualizedBoundCounter)
compileCounter rules = f
  where
  f "Calls"                      m            = boundMatching rules countCalls m
  f "DeclaresAttribute"          m            = boundMatching rules countAttributes m
  f "DeclaresClass"              m            = boundMatching rules countClasses m
  f "DeclaresFunction"           m            = boundMatching rules countFunctions m
  f "DeclaresInterface"          m            = boundMatching rules countInterfaces m
  f "DeclaresMethod"             m            = boundMatching rules countMethods m
  f "DeclaresObject"             m            = boundMatching rules countObjects m
  f "DeclaresProcedure"          m            = boundMatching rules countProcedures m
  f "DeclaresVariable"           m            = boundMatching rules countVariables m
  f "Returns"                    m            = plainMatching rules countReturns m
  f "Uses"                       E.Unmatching = bound countUses
  f "UsesFor"                    m            = plainMatching rules countFors m
  f "UsesForEach"                m            = plainMatching rules countForEaches m
  f "UsesForLoop"                m            = plainMatching rules countForLoops m
  f "UsesIf"                     m            = plainMatching rules countIfs m
  f "UsesLambda"                 m            = plainMatching rules countLambdas m
  f "UsesPrint"                  m            = plainMatching rules countPrints m
  f "UsesRepeat"                 m            = plainMatching rules countRepeats m
  f "UsesSwitch"                 m            = plainMatching rules countSwitches m
  f "UsesWhile"                  m            = plainMatching rules countWhiles m
  f "UsesYield"                  m            = plainMatching rules countYiels m
  f (primitiveUsage -> Just p)   E.Unmatching = plain (countUsesPrimitive p)
  f _                            _            = Nothing


compileInspection :: AutocorrectionRules -> String -> E.Matcher -> Maybe ContextualizedBoundInspection
compileInspection rules = f
  where
  f "Assigns"                          m              = boundMatching rules assignsMatching m
  f "Calls"                            m              = boundMatching rules callsMatching m
  f "Declares"                         E.Unmatching   = bound declares
  f "DeclaresAttribute"                m              = boundMatching rules declaresAttributeMatching m
  f "DeclaresClass"                    m              = boundMatching rules declaresClassMatching m
  f "DeclaresComputation"              E.Unmatching   = bound declaresComputation
  f "DeclaresComputationWithArity0"    E.Unmatching   = bound (declaresComputationWithArity 0)
  f "DeclaresComputationWithArity1"    E.Unmatching   = bound (declaresComputationWithArity 1)
  f "DeclaresComputationWithArity2"    E.Unmatching   = bound (declaresComputationWithArity 2)
  f "DeclaresComputationWithArity3"    E.Unmatching   = bound (declaresComputationWithArity 3)
  f "DeclaresComputationWithArity4"    E.Unmatching   = bound (declaresComputationWithArity 4)
  f "DeclaresComputationWithArity5"    E.Unmatching   = bound (declaresComputationWithArity 5)
  f "DeclaresEntryPoint"               m              = boundMatching rules declaresEntryPointMatching m
  f "DeclaresEnumeration"              E.Unmatching   = bound declaresEnumeration
  f "DeclaresFact"                     E.Unmatching   = bound declaresFact
  f "DeclaresFunction"                 m              = boundMatching rules declaresFunctionMatching m
  f "DeclaresInterface"                m              = boundMatching rules declaresInterfaceMatching m
  f "DeclaresMethod"                   m              = boundMatching rules declaresMethodMatching m
  f "DeclaresObject"                   m              = boundMatching rules declaresObjectMatching m
  f "DeclaresPredicate"                E.Unmatching   = bound declaresPredicate
  f "DeclaresProcedure"                m              = boundMatching rules declaresProcedureMatching m
  f "DeclaresRecursively"              E.Unmatching   = bound declaresRecursively
  f "DeclaresRule"                     E.Unmatching   = bound declaresRule
  f "DeclaresSuperclass"               E.Unmatching   = bound declaresSuperclass
  f "DeclaresTypeAlias"                E.Unmatching   = bound declaresTypeAlias
  f "DeclaresTypeSignature"            E.Unmatching   = bound declaresTypeSignature
  f "DeclaresVariable"                 m              = boundMatching rules declaresVariableMatching m
  f "Delegates"                        E.Unmatching   = contextualizedBound delegates'
  f "Implements"                       E.Unmatching   = bound implements
  f "Includes"                         E.Unmatching   = bound includes
  f "Inherits"                         E.Unmatching   = bound inherits
  f "Instantiates"                     E.Unmatching   = bound instantiates
  f "IsClass"                          E.Unmatching   = bound isClass
  f "IsDeclaration"                    E.Unmatching   = bound isDeclaration
  f "IsFunction"                       E.Unmatching   = bound isFunction
  f "IsLValue"                         E.Unmatching   = bound isLValue
  f "IsVariable"                       E.Unmatching   = bound isVariable
  f "IsNil"                            E.Unmatching   = plain isNil
  f "IsSelf"                           E.Unmatching   = plain isSelf
  f "IsLiteral"                        E.Unmatching   = plain isLiteral
  f "Raises"                           E.Unmatching   = bound raises
  f "Rescues"                          E.Unmatching   = bound rescues
  f "Returns"                          m              = plainMatching rules returnsMatching m
  f "SubordinatesDeclarationsTo"       E.Unmatching   = bound subordinatesDeclarationsTo
  f "SubordinatesDeclarationsToEntryPoint" E.Unmatching = plain subordinatesDeclarationsToEntryPoint
  f "TypesAs"                          E.Unmatching   = bound typesAs
  f "TypesParameterAs"                 E.Unmatching   = bound typesParameterAs
  f "TypesReturnAs"                    E.Unmatching   = bound typesReturnAs
  f "Uses"                             E.Unmatching   = bound uses
  f "UsesAnonymousVariable"            E.Unmatching   = plain usesAnonymousVariable
  f "UsesComposition"                  E.Unmatching   = plain usesComposition
  f "UsesComprehension"                E.Unmatching   = f "UsesForComprehension" E.Unmatching
  f "UsesConditional"                  E.Unmatching   = plain usesConditional
  f "UsesDyamicPolymorphism"           E.Unmatching   = contextual usesDyamicPolymorphism'
  f "UsesDynamicMethodOverload"        E.Unmatching   = plain usesDynamicMethodOverload
  f "UsesExceptionHandling"            E.Unmatching   = plain usesExceptionHandling
  f "UsesExceptions"                   E.Unmatching   = plain usesExceptions
  f "UsesFindall"                      E.Unmatching   = plain usesFindall
  f "UsesFor"                          m              = plainMatching rules usesForMatching m
  f "UsesForall"                       E.Unmatching   = plain usesForall
  f "UsesForComprehension"             E.Unmatching   = plain usesForComprehension
  f "UsesForeach"                      m              = plainMatching rules usesForEachMatching m
  f "UsesForLoop"                      m              = plainMatching rules usesForLoopMatching m
  f "UsesGuards"                       E.Unmatching   = plain usesGuards
  f "UsesIf"                           m              = plainMatching rules usesIfMatching m
  f "UsesInheritance"                  E.Unmatching   = plain usesInheritance
  f "UsesLambda"                       m              = plainMatching rules usesLambdaMatching m
  f "UsesLogic"                        E.Unmatching   = plain usesLogic
  f "UsesLoop"                         E.Unmatching   = plain usesLoop
  f "UsesMath"                         E.Unmatching   = plain usesMath
  f "UsesMixins"                       E.Unmatching   = plain usesMixins
  f "UsesNot"                          E.Unmatching   = plain usesNot
  f "UsesObjectComposition"            E.Unmatching   = plain usesObjectComposition
  f "UsesPatternMatching"              E.Unmatching   = plain usesPatternMatching
  f "UsesPrint"                        m              = plainMatching rules usesPrintMatching m
  f "UsesRepeat"                       m              = plainMatching rules usesRepeatMatching m
  f "UsesStaticMethodOverload"         E.Unmatching   = plain usesStaticMethodOverload
  f "UsesStaticPolymorphism"           E.Unmatching   = contextual usesStaticPolymorphism'
  f "UsesSwitch"                       m              = plainMatching rules usesSwitchMatching m
  f "UsesTemplateMethod"               E.Unmatching   = plain usesTemplateMethod
  f "UsesType"                         E.Unmatching   = bound usesType
  f "UsesWhile"                        m              = plainMatching rules usesWhileMatching m
  f "UsesYield"                        m              = plainMatching rules usesYieldMatching m
  f (primitiveDeclaration -> Just p)   E.Unmatching   = plain (declaresPrimitive p)
  f (primitiveUsage -> Just p)         E.Unmatching   = plain (usesPrimitive p)
  f (primitiveEssence -> Just p)       E.Unmatching   = plain (isPrimitive p)
  f _                                  _              = Nothing

primitiveEssence = decodeIsInspection
primitiveUsage = decodeUsageInspection
primitiveDeclaration = decodeDeclarationInspection

contextual :: ContextualizedConsult a -> Maybe (ContextualizedBoundConsult a)
contextual = Just . contextualizedBind

contextualizedBound :: ContextualizedBoundConsult a -> Maybe (ContextualizedBoundConsult a)
contextualizedBound = Just

plain :: Consult a -> Maybe (ContextualizedBoundConsult a)
plain = Just . contextualizedBind . contextualize

bound :: BoundConsult a -> Maybe (ContextualizedBoundConsult a)
bound = Just . boundContextualize

boundMatching :: AutocorrectionRules -> (Matcher -> BoundConsult a) -> E.Matcher -> Maybe (ContextualizedBoundConsult a)
boundMatching rules f m = bound (f (compileMatcher rules m))

plainMatching :: AutocorrectionRules -> (Matcher -> Consult a) -> E.Matcher -> Maybe (ContextualizedBoundConsult a)
plainMatching rules f m = plain (f (compileMatcher rules m))

compileMatcher :: AutocorrectionRules -> E.Matcher -> Matcher
compileMatcher rules (E.Matching clauses) = compileClauses rules clauses
compileMatcher _     _                    = const True

compileClauses :: AutocorrectionRules -> [E.Clause] -> Matcher
compileClauses rules = withEvery . f
  where
    f :: [E.Clause] -> [Inspection]
    f (E.IsAnything:args)       = isAnything : (f args)
    f (E.IsChar value:args)     = isChar value : (f args)
    f (E.IsFalse:args)          = isBool False : (f args)
    f (E.IsLiteral:args)        = isLiteral : (f args)
    f (E.IsLogic:args)          = usesLogic : (f args)
    f (E.IsMath:args)           = usesMath : (f args)
    f (E.IsNil:args)            = isNil : (f args)
    f (E.IsNonliteral:args)     = isNonliteral : (f args)
    f (E.IsSelf:args)           = isSelf : (f args)
    f (E.IsTrue:args)           = isBool True : (f args)
    f (E.IsNumber value:args)   = isNumber value : (f args)
    f (E.IsString value:args)   = isString value : (f args)
    f (E.IsSymbol value:args)   = isSymbol value : (f args)
    f (E.That expectation:args) = compileTopQuery' rules expectation : (f args)
    f []                        = []
