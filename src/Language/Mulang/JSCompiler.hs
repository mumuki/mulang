{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Language.Mulang.JSCompiler (toJS) where

import           Language.Mulang.Ast
import           Data.Text(pack, unpack, intercalate, empty, toLower, Text)
import           NeatInterpolation

-- | Compiles a Mulang AST to an executable JS source.
-- | If the AST is not representable in JS (for example, if it contains Facts or other non-OOP
-- | abstractions), the function will yield Nothing.
-- | The generated code asumes it will be appended to another source string containing all primitive definitions
-- | (some sort of JRE for the parsed languaje).
-- | Notice that, since some semantics of the original code's language are lost during the parsing,
-- | executing this code might not be equivalent to executing the original sources.
-- | It might be reasonable to drop this whole implementation in the future and replace it with a
-- | direct interpretation of the Ast.
-- |
-- | TODO: A proper implementation of this would escape all JS reserved (or context relevant) words on the AST, (for
-- | example, any variable named "arguments" on the AST would misbehave on the transpiled JS).
toJS :: Expression -> Maybe Code
toJS = fmap unpack . compile

class Compilable a where
  compile :: a -> Maybe Text
  compileAll :: [a] -> String -> Maybe Text
  compileAll compilables separator = fmap (intercalate (pack separator)) . sequence . map compile $ compilables

instance Compilable Expression where
  -- Generates a call to the primitive MuNumber constructor, passing a JS Double as argument.
  compile (MuNumber _number) = do
    let number = pack $ show _number
    return [text| new MuNumber($number) |]

  -- Generates a call to the primitive MuBool constructor, passing a JS Boolean as argument.
  compile (MuBool _bool) = do
    let bool = toLower . pack $ show _bool
    return [text| new MuBool($bool) |]

  -- Generates a call to the primitive MuString constructor, passing a JS String as argument.
  compile (MuString _string) = do
    let string = pack _string
    return [text| new MuString("$string") |]

  -- Generates a call to the primitive MuSymbol constructor, passing a JS String as argument.
  compile (MuSymbol _string) = do
    let string = pack _string
    return [text| new MuSymbol("$string") |]

  -- Generates a call to the primitive MuTuple constructor, passing a JS Array of MuObjects as argument.
  compile (MuTuple _values) = do
    values <- compileAll _values ", "
    return [text| new MuTuple([$values]) |]

  -- Generates a call to the primitive MuList constructor, passing a JS Array of MuObjects as argument.
  compile (MuList _values) = do
    values <- compileAll _values ", "
    return [text| new MuList([$values]) |]

  -- Generates a named JS function with the EntryPoint's body.
  compile (EntryPoint _name _body) = do
    let name = pack _name
    body <- compile _body
    return [text| function $name() { return $body } |]

  compile (Return _expression) = do
    expression <- compile _expression
    return [text| return $expression |]

  -- Generates a named JS function that dynamically dispatches the Function's Equations based on the received arguments.
  -- A primitive MuPatternMatchError exception is raised if no equation matches the cases.
  compile (Function _name _equations) = do
    let name = pack _name
    equations <- compileAll _equations " else "
    return [text| function $name() { $equations throw new MuPatternMatchError() } |]

  -- Generates a named JS function that dynamically dispatches the Function's Equations based on the received arguments.
  -- A primitive MuPatternMatchError exception is raised if no equation matches the cases.
  compile (Procedure _name _equations) = do
    let name = pack _name
    equations <- compileAll _equations " else "
    return [text| function $name() { $equations throw new MuPatternMatchError() } |]

  -- Generates a JS variable initializing it to the Variables value.
  compile (Variable _name _value) = do
    let name = pack _name
    value <- compile _value
    return [text| var $name = $value |]

  -- Generates a JS assignment sentence.
  compile (Assignment _name _value) = do
    let name = pack _name
    value <- compile _value
    return [text| $name = $value |]

  -- Generates a JS reference name.
  -- TODO: This should probably be escaped for JS keywords.
  compile (Reference _name) = do
    let name = pack _name
    return [text| $name |]

  -- Generates a JS direct application.
  compile (Application _target _arguments) = do
    target <- compile _target
    arguments <- compileAll _arguments ", "
    return [text| $target($arguments) |]

  -- Generates a JS by-name access on the receiver (in order to support invalid JS selectors) and appends a
  -- direct application of the arguments.
  compile (Send _receiver _selector _arguments) = do
    receiver <- compile _receiver
    selector <- compile _selector
    arguments <- compileAll _arguments ", "
    return [text| $receiver['${selector}']($arguments) |]

  -- Generates a JS object instantiation with the given arguments.
  compile (New _name _arguments) = do
    let name = pack _name
    arguments <- compileAll _arguments ", "
    return [text| new $name($arguments) |]

  -- Generates a JS throw, wrapped in an anonymous function to make it an expression.
  compile (Raise _exception) = do
    exception <- compile _exception
    return [text| function(){ throw $exception }() |]

  -- Generates a call to JS's console.log. This might also have been implemented as a call to a primitive... Just saying.
  compile (Print _expression) = do
    expression <- compile _expression
    return [text| console.log($expression) |]

  -- Generates a string to be used as identifier in a message send.
  -- This identifier should be implemented as a primitive on the MuObject primitive class.
  compile Equal = do return [text| === |]

  -- Generates a string to be used as identifier in a message send.
  -- This identifier should be implemented as a primitive on the MuObject primitive class.
  compile NotEqual = do return [text| !== |]

  -- Generates the JS this meta-reference.
  compile Self = do return [text| this |]

  -- Generates a call the the MuNull primitive class' constructor.
  -- (this does not mean there has to be many MuNull instances, the constructor can just return a singleton).
  compile MuNull = do return [text| new MuNull() |]

  -- TypeSignatures are ignored.
  compile (TypeSignature _ _ _)  = do return empty

  -- TypeAliases are ignored (we can't do anything without the aliased type).
  compile (TypeAlias _)  = do return empty

  -- Records are ignored (we can't do anything without the body).
  compile (Record _)  = do return empty

  compile _ = Nothing

{-
data Expression
    | Method Identifier [Equation]
    | EqualMethod [Equation]
    | HashMethod [Equation]
    | Attribute Identifier Expression

    | Object Identifier Expression
    -- ^ Object oriented programming global, named object declaration,
    --   composed by a name and a body

    | Class Identifier (Maybe Identifier) Expression
    -- ^ Object oriented programming global, class declaration,
    --   composed by a name, an optional superclass, implemented interfaces and a body
 
    | Enumeration Identifier [Identifier]
    -- ^ Imperative named enumeration of values
 
    | Interface Identifier [Identifier] Expression
    -- ^ Object oriented programming global interface or contract declaration,
    --   composed by a name, superinterfaces and a body
 

    | Implement Identifier
    -- ^ Object oriented instantiation, interface implementation
    | Include Identifier
    -- ^ Object oriented instantiation, mixin inclusion

    | Sequence [Expression]
    -- ^ Generic sequence of statements


    | Lambda [Pattern] Expression
    | If Expression Expression Expression

    | While Expression Expression
    -- ^ Imperative programming conditional repetition control structure, composed by a condition and a body
    | Repeat Expression Expression
    -- ^ Imperative programming fixed repetition control structure, composed by a repetition count expression, and a body
    | Match Expression [Equation]
    | Switch Expression [(Expression, Expression)]
    | Try Expression [(Pattern, Expression)] Expression
    -- ^ Generic try expression, composed by a body, a list of exception-handling patterns and statments, and a finally expression
    | MuObject Expression
    -- ^ Object oriented unnamed object literal

-}

instance Compilable Equation where
  -- | Compiles a Mulang Equation to a JS if statement.
  -- | The generated code is meant to be evaluated in the context of a container function, where the arguments variable
  -- | is available. The if condition will test the context's arguments against the expected patterns and either execute
  -- | and exit the context or avoid execution and continue context as is.
  -- | The context is expected to fail after all equations are tested unsuccessfully.
  -- | The Equation body is expected to explicitly contain a Return expression.
  compile (Equation argumentPatterns _body) = do
    let argumentCount = pack . show . length $ argumentPatterns
    let indexedPatterns = zip [0..] argumentPatterns
    matchesPatterns <- fmap (intercalate (pack " && ")) . sequence $ Just (pack "true") : map compilePatternCondition indexedPatterns
    argumentNames <- fmap (intercalate (pack "")) . sequence $ map compileParameterNameAssignation indexedPatterns
    body <- compileBody _body
    return [text| if(arguments.length === $argumentCount && $matchesPatterns){ $argumentNames $body } |]
    where
      -- | Compiles a pair (Index, Mulang Pattern) to a JS boolean expression over the arguments variable.
      -- | A primitive function isInstanceOf is called for TypePatterns.
      -- | TODO: Complete missing scenarios.
      compilePatternCondition :: (Int, Pattern) -> Maybe Text
      compilePatternCondition (_, (VariablePattern _)) = Just [text| true |]
      compilePatternCondition (_, (WildcardPattern)) = Just [text| true |]
      compilePatternCondition (_n, (LiteralPattern _literal)) =  do
       let n = pack . show $ _n
       let literal = pack . show $ _literal
       return [text| arguments[$n] === $literal |]
      compilePatternCondition (_n, (TypePattern _typeName)) = do
       let n = pack . show $ _n
       let typeName = pack _typeName
       return [text| isInstanceOf(arguments[$n], $typeName) |]
      compilePatternCondition _ = Nothing

      -- | Compiles a pair (Index, Mulang Pattern) to a JS code fragment that assigns arguments to the name the body uses
      -- | to reference them.
      -- | TODO: Complete missing scenarios.
      compileParameterNameAssignation :: (Int, Pattern) -> Maybe Text
      compileParameterNameAssignation (_n, (VariablePattern _name)) = do
        let n = pack . show $ _n
        let name = pack _name
        return [text| var $name = arguments[$n]; |]
      compileParameterNameAssignation (_, (LiteralPattern _literal)) = Just [text| |]
      compileParameterNameAssignation (_, (WildcardPattern)) = Just [text| |]
      compileParameterNameAssignation (_, (TypePattern _typeName)) = Just [text|  |] --TODO: The variable name is missing. How do you identify the parameter in the body?
      compileParameterNameAssignation _ = Nothing

      -- | Compiles a Mulang EquationBody to a JS code fragment. The generated code is intended to be contained within an
      -- | Equation and should only be evaluated once the pattern has been checked.
      compileBody :: EquationBody -> Maybe Text
      compileBody (UnguardedBody expression) = compile expression
      compileBody (GuardedBody _cases) = do
        cases <- fmap (intercalate (pack " else ")) . sequence $ map (\(_condition, _expression)-> do
                   condition <- compile _condition
                   expression <- compile _expression
                   return [text| if($condition) { $expression } |]
                 ) _cases
        return [text| $cases throw new MuPatternMatchError() |]