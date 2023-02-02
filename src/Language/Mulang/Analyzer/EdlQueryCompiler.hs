{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Analyzer.EdlQueryCompiler(
  compileTopQuery) where

import Data.Char (toUpper)
import Data.Count (encode)
import Data.Function.Extra (orElse, andAlso, never)

import Language.Mulang
import Language.Mulang.Consult (Consult)
import Language.Mulang.Counter (plus)
import Language.Mulang.Inspector.Primitive (atLeast, atMost, exactly)
import Language.Mulang.Inspector.Literal (isNil, isNumber, isBool, isChar, isString, isSymbol, isSelf, isLiteral)
import Language.Mulang.Analyzer.Synthesizer (decodeIsInspection, decodeCallsInspection, decodeUsageInspection, decodeDeclarationInspection)

import qualified Language.Mulang.Edl.Expectation as E

import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)

type Scope = (ContextualizedInspection -> ContextualizedInspection, IdentifierPredicate -> IdentifierPredicate)

compileTopQuery :: E.Query -> Inspection
compileTopQuery = fromMaybe (const True) . compileQuery

compileQuery :: E.Query -> Maybe Inspection
compileQuery (E.Decontextualize query) = compileCQuery id query >>= (return . decontextualize)
compileQuery (E.Within name query)     | (scope, p) <- compileWithin name  = fmap (decontextualize.scope) (compileCQuery p query)
compileQuery (E.Through name query)    | (scope, p) <- compileThrough name = fmap (decontextualize.scope) (compileCQuery p query)
compileQuery (E.Not query)             = fmap never (compileQuery query)
compileQuery (E.And query1 query2)     = andAlso <$> (compileQuery query1) <*> (compileQuery query2)
compileQuery (E.Or query1 query2)      = orElse <$> (compileQuery query1) <*> (compileQuery query2)

compileWithin, compileThrough :: String -> Scope
compileWithin name  = scopeFor scopedList name
compileThrough name = scopeFor transitiveList name

scopeFor :: ([Identifier] -> Inspection -> Inspection) -> Identifier -> Scope
scopeFor f name = (contextualized (f names), andAlso (except (last names)))
  where names = splitOn "." name

compileCQuery :: (IdentifierPredicate -> IdentifierPredicate) -> E.CQuery -> Maybe ContextualizedInspection
compileCQuery pm (E.Inspection i p m) = ($ (compilePredicate pm p))         <$> compileInspection (compileVerb i) m
compileCQuery pm (E.AtLeast n q)      = contextualized (atLeast (encode n)) <$> compileTQuery pm q
compileCQuery pm (E.AtMost n q)       = contextualized (atMost (encode n))  <$> compileTQuery pm q
compileCQuery pm (E.Exactly n q)      = contextualized (exactly (encode n)) <$> compileTQuery pm q
compileCQuery pm (E.CNot q)           = contextualized never                <$> compileCQuery pm q
compileCQuery pm (E.CAnd q1 q2)       = contextualized2 andAlso             <$> compileCQuery pm q1 <*> compileCQuery pm q2
compileCQuery pm (E.COr q1 q2)        = contextualized2 orElse              <$> compileCQuery pm q1 <*> compileCQuery pm q2

compileTQuery :: (IdentifierPredicate -> IdentifierPredicate) -> E.TQuery -> Maybe ContextualizedCounter
compileTQuery pm (E.Counter i p m) = ($ (compilePredicate pm p)) <$> compileCounter (compileVerb i) m
compileTQuery pm (E.Plus q1 q2)    = contextualized2 plus        <$> (compileTQuery pm q1) <*> (compileTQuery pm q2)

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

compileCounter :: String -> E.Matcher -> Maybe (ContextualizedBoundCounter)
compileCounter = f
  where
  f "Calls"                      m            = boundMatching countCalls m
  f "DeclaresAttribute"          m            = boundMatching countAttributes m
  f "DeclaresClass"              m            = boundMatching countClasses m
  f "DeclaresFunction"           m            = boundMatching countFunctions m
  f "DeclaresInterface"          m            = boundMatching countInterfaces m
  f "DeclaresMethod"             m            = boundMatching countMethods m
  f "DeclaresObject"             m            = boundMatching countObjects m
  f "DeclaresProcedure"          m            = boundMatching countProcedures m
  f "DeclaresVariable"           m            = boundMatching countVariables m
  f "Returns"                    m            = plainMatching countReturns m
  f "Uses"                       E.Unmatching = bound countUses
  f "UsesFor"                    m            = plainMatching countFors m
  f "UsesForEach"                m            = plainMatching countForEaches m
  f "UsesForLoop"                m            = plainMatching countForLoops m
  f "UsesIf"                     m            = plainMatching countIfs m
  f "UsesLambda"                 m            = plainMatching countLambdas m
  f "UsesPrint"                  m            = plainMatching countPrints m
  f "UsesRepeat"                 m            = plainMatching countRepeats m
  f "UsesSwitch"                 m            = plainMatching countSwitches m
  f "UsesWhile"                  m            = plainMatching countWhiles m
  f "UsesYield"                  m            = plainMatching countYiels m
  f (primitiveCalls -> Just p)   m            = plainMatching (\m -> countPrimitiveCalls m p) m
  f (primitiveUsage -> Just p)   E.Unmatching = plain (countUsesPrimitive p)
  f _                            _            = Nothing


compileInspection :: String -> E.Matcher -> Maybe ContextualizedBoundInspection
compileInspection = f
  where
  f "Assigns"                          m              = boundMatching assignsMatching m
  f "Calls"                            m              = boundMatching callsMatching m
  f "Declares"                         E.Unmatching   = bound declares
  f "DeclaresAttribute"                m              = boundMatching declaresAttributeMatching m
  f "DeclaresClass"                    m              = boundMatching declaresClassMatching m
  f "DeclaresComputation"              E.Unmatching   = bound declaresComputation
  f "DeclaresComputationWithArity0"    E.Unmatching   = bound (declaresComputationWithArity 0)
  f "DeclaresComputationWithArity1"    E.Unmatching   = bound (declaresComputationWithArity 1)
  f "DeclaresComputationWithArity2"    E.Unmatching   = bound (declaresComputationWithArity 2)
  f "DeclaresComputationWithArity3"    E.Unmatching   = bound (declaresComputationWithArity 3)
  f "DeclaresComputationWithArity4"    E.Unmatching   = bound (declaresComputationWithArity 4)
  f "DeclaresComputationWithArity5"    E.Unmatching   = bound (declaresComputationWithArity 5)
  f "DeclaresEntryPoint"               m              = boundMatching declaresEntryPointMatching m
  f "DeclaresEnumeration"              E.Unmatching   = bound declaresEnumeration
  f "DeclaresFact"                     E.Unmatching   = bound declaresFact
  f "DeclaresFunction"                 m              = boundMatching declaresFunctionMatching m
  f "DeclaresInterface"                m              = boundMatching declaresInterfaceMatching m
  f "DeclaresMethod"                   m              = boundMatching declaresMethodMatching m
  f "DeclaresObject"                   m              = boundMatching declaresObjectMatching m
  f "DeclaresPredicate"                E.Unmatching   = bound declaresPredicate
  f "DeclaresProcedure"                m              = boundMatching declaresProcedureMatching m
  f "DeclaresRecursively"              E.Unmatching   = bound declaresRecursively
  f "DeclaresRule"                     E.Unmatching   = bound declaresRule
  f "DeclaresSuperclass"               E.Unmatching   = bound declaresSuperclass
  f "DeclaresTypeAlias"                E.Unmatching   = bound declaresTypeAlias
  f "DeclaresTypeSignature"            E.Unmatching   = bound declaresTypeSignature
  f "DeclaresVariable"                 m              = boundMatching declaresVariableMatching m
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
  f "Returns"                          m              = plainMatching returnsMatching m
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
  f "UsesDynamicPolymorphism"          E.Unmatching   = contextual usesDynamicPolymorphism'
  f "UsesDynamicMethodOverload"        E.Unmatching   = plain usesDynamicMethodOverload
  f "UsesExceptionHandling"            E.Unmatching   = plain usesExceptionHandling
  f "UsesExceptions"                   E.Unmatching   = plain usesExceptions
  f "UsesFindall"                      E.Unmatching   = plain usesFindall
  f "UsesFor"                          m              = plainMatching usesForMatching m
  f "UsesForall"                       E.Unmatching   = plain usesForall
  f "UsesForComprehension"             E.Unmatching   = plain usesForComprehension
  f "UsesForeach"                      m              = plainMatching usesForEachMatching m
  f "UsesForLoop"                      m              = plainMatching usesForLoopMatching m
  f "UsesGuards"                       E.Unmatching   = plain usesGuards
  f "UsesIf"                           m              = plainMatching usesIfMatching m
  f "UsesInheritance"                  E.Unmatching   = plain usesInheritance
  f "UsesLambda"                       m              = plainMatching usesLambdaMatching m
  f "UsesLogic"                        E.Unmatching   = plain usesLogic
  f "UsesLoop"                         E.Unmatching   = plain usesLoop
  f "UsesMath"                         E.Unmatching   = plain usesMath
  f "UsesMixins"                       E.Unmatching   = plain usesMixins
  f "UsesNot"                          E.Unmatching   = plain usesNot
  f "UsesObjectComposition"            E.Unmatching   = plain usesObjectComposition
  f "UsesPatternMatching"              E.Unmatching   = plain usesPatternMatching
  f "UsesPrint"                        m              = plainMatching usesPrintMatching m
  f "UsesRepeat"                       m              = plainMatching usesRepeatMatching m
  f "UsesStaticMethodOverload"         E.Unmatching   = plain usesStaticMethodOverload
  f "UsesStaticPolymorphism"           E.Unmatching   = contextual usesStaticPolymorphism'
  f "UsesSwitch"                       m              = plainMatching usesSwitchMatching m
  f "UsesTemplateMethod"               E.Unmatching   = plain usesTemplateMethod
  f "UsesType"                         E.Unmatching   = bound usesType
  f "UsesWhile"                        m              = plainMatching usesWhileMatching m
  f "UsesYield"                        m              = plainMatching usesYieldMatching m
  f (primitiveCalls -> Just p)         m              = plainMatching (\m -> callsPrimitiveMatching m p) m
  f (primitiveDeclaration -> Just p)   E.Unmatching   = plain (declaresPrimitive p)
  f (primitiveUsage -> Just p)         E.Unmatching   = plain (usesPrimitive p)
  f (primitiveEssence -> Just p)       E.Unmatching   = plain (isPrimitive p)
  f _                                  _              = Nothing

primitiveEssence = decodeIsInspection
primitiveUsage = decodeUsageInspection
primitiveDeclaration = decodeDeclarationInspection
primitiveCalls = decodeCallsInspection

contextual :: ContextualizedConsult a -> Maybe (ContextualizedBoundConsult a)
contextual = Just . contextualizedBind

contextualizedBound :: ContextualizedBoundConsult a -> Maybe (ContextualizedBoundConsult a)
contextualizedBound = Just

plain :: Consult a -> Maybe (ContextualizedBoundConsult a)
plain = Just . contextualizedBind . contextualize

bound :: BoundConsult a -> Maybe (ContextualizedBoundConsult a)
bound = Just . boundContextualize

boundMatching :: (Matcher -> BoundConsult a) -> E.Matcher -> Maybe (ContextualizedBoundConsult a)
boundMatching f m = bound (f (compileMatcher m))

plainMatching :: (Matcher -> Consult a) -> E.Matcher -> Maybe (ContextualizedBoundConsult a)
plainMatching f m = plain (f (compileMatcher m))

compileMatcher :: E.Matcher -> Matcher
compileMatcher (E.Matching clauses) = compileClauses clauses
compileMatcher _                    = const True

compileClauses :: [E.Clause] -> Matcher
compileClauses = withEvery . f
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
    f (E.That expectation:args) = compileTopQuery expectation : (f args)
    f []                        = []
