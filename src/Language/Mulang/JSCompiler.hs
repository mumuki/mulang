{-# LANGUAGE QuasiQuotes, OverloadedStrings, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Language.Mulang.JSCompiler (toJS) where

import           Language.Mulang.Ast
import           Data.Text(pack, unpack, intercalate, empty, toLower, Text)
import           Data.Maybe
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

  -- Generates a JS direct application of an anonymous function that throws an instance of MuReturn primitive class.
  -- This is done to ensure that the return forces the exit of the contexts of abstractions represented with functions.
  compile (Return _result) = do
    result <- compile _result
    return [text| function(){ throw new MuReturn($result) }() |]

  -- Generates a named JS function that dynamically dispatches the Function's Equations based on the received arguments.
  -- A primitive MuPatternMatchError exception is raised if no equation matches the cases.
  -- The MuReturn primitive error is catched to allow inner expressions to exit this context eagerly.
  compile (Function _name _equations) = do
    let name = pack _name
    equations <- compileAll _equations " else "
    return [text| function $name() {
                    try { $equations throw new MuPatternMatchError() }
                    catch($$error) { if($$error.constructor === MuReturn) { return $$error.value } else { throw $$error } } }
           |]

  compile (Procedure _name _equations) = compile $ Function _name _equations
  compile (Lambda _argumentPatterns _body) = compile $ Function "" [Equation _argumentPatterns $ UnguardedBody _body]

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

  -- Generates a JS undefined value if the sequence is empty.
  -- If the sequence is not empty, generates an invocation to an anonymous function where each line is one sequence member.
  -- The last element of the sequence is returned as the function response, ignoring any intermediate result, but applying effects.
  compile (Sequence []) = do return [text| undefined |]
  compile (Sequence _expressions) = let _lastExpression = last _expressions in do
    initialExpressions <- compileAll (init _expressions) "; "
    lastExpression <- compile _lastExpression
    return [text| function(){ $initialExpressions; return $lastExpression }() |]

  -- Generates a sequence of JS variable definitions, one for each Enum member (assigning each to an integer value) and
  -- one for the Enum itself, pointed to a trivial JS map where each Enum member is a key.
  compile (Enumeration _name _members) = do
    let name    = pack _name
    let members = map pack _members
    let values  = zip (map (pack.show) [0..length members]) members
    let compiledValues  = intercalate (pack ", ") $ map (\(i, member) -> [text| $member: $i |]) values
    let compiledMembers = intercalate (pack ";\n") $ map (\member -> [text| var $member = $name['${member}'] |]) members
    return [text| var $name = { $compiledValues }; $compiledMembers; |]

  -- Generates a JS if statement wrapped in an anonymous function application in order to make it an expression.
  compile (If _condition _positiveCase _negativeCase) = do
    condition <- compile _condition
    positiveCase <- compile _positiveCase
    negativeCase <- compile _negativeCase
    return [text| function(){ if($condition) { return $positiveCase } else { return $negativeCase } }() |]

  -- Generates a JS while statement wrapped in an anonymous function application in order to make it an expression.
  compile (While _condition _body) = do
    condition <- compile _condition
    body <- compile _body
    return [text| function(){ while($condition) { $body } }() |]

  -- Generates a JS for statement wrapped in an anonymous function application in order to make it an expression.
  compile (Repeat _amount _body) = do
    amount <- compile _amount
    body <- compile _body
    return [text| function(){ for(var $$$$i=0; $$$$i < $amount; $$$$i++) { $body } }() |]

  -- Generates a JS try statement wrapped in an anonymous function application in order to make it an expression.
  -- The code tries to emulate the most common behaviour of try clauses (being this that body, catches and always execute
  -- on isolated contexts and, always gets evaluated before catches).
  compile (Try _body _catches _always) = do
    body <- compile _body
    catches <- compileAll _catches " else "
    always <- compile _always
    return [text|
             function(){
               var $$response;
               try { $$response = $body }
               catch($$error){ function(){ $always }(); arguments = [$$error]; $catches; throw $$error }
               function{ $always }();
               return $$response
             }()
           |]

  -- Generates a JS if-else if statement wrapped in an anonymous function application in order to make it an expression.
  compile (Switch _value _cases) = do
    value <- compile _value
    cases <- fmap (intercalate $ pack " else ") . sequence $ map compileCase _cases
    return [text| function(){ $cases }($value) |]

    where compileCase (_target, _body) = do
            target <- compile _target
            body   <- compile _body
            return [text| if(arguments[0] === $target) { return $body } |]

  -- Generates a JS function for the class. The superclass assignation is lazily applied to compensate for the lack of
  -- hoisting.
  compile (Class _name _superclassName _body) = do
    let name = pack _name
    let superclassName = pack $ fromMaybe "MuObject" _superclassName
    body <- compileBody _body
    return [text| function $name() {
                    this.constructor.prototype = Object.create($superclassName.prototype);
                    $superclassName.call(this);
                    $body
                  }
           |]
    where compileBody :: Expression -> Maybe Text
          compileBody (Sequence expressions) = compileAll expressions ";\n"
          compileBody MuNull                 = Just [text| |]
          compileBody expression             = compile expression

  -- Generates a JS new of an instance of an anonymous class.
  -- I'm not sure how object's superclass is supposed to be represented, but Include of classes should work.
  compile (MuObject _body) = do
    singletonClass <- compile $ Class "" Nothing _body
    return[text| new $singletonClass() |]

  -- Generates a JS variable assignation pointing to an anonymous instance.
  compile (Object _name _body) = do
    let name = pack _name
    object <- compile $ MuObject _body
    return [text| var $name = $object |]

  -- Generates a JS assignation statement that overrides the context's constructor's prototype slot named as the method
  -- with a new function for the method.
  -- By-name access is used in order to support invalid JS selectors.
  -- The generated code is meant to be evaluated in the context of a container "class" function.
  compile (Method _name _equations) = do
    let name = pack _name
    body <- compile $ Function "" _equations
    return [text| this.constructor.prototype['${name}'] = $body |]

  compile (EqualMethod _equations) = compile $ Method "===" _equations
  compile (HashMethod _equations) = compile $ Method "hash" _equations

  -- Generates a JS assignation statement that overrides the context's slot named as the attribute.
  -- By-name access is used in order to support invalid JS selectors.
  -- The generated code is meant to be evaluated in the context of a container "class" function.
  compile (Attribute _name _value) = do
    let name = pack _name
    value <- compile _value
    return [text| this['${name}'] = $value |]

  -- Generates a JS assignation statement that replaces the current context's constructor's prototype with a copy of
  -- itself merged with the mixin's prototype.
  -- It also calls the mixin initialization on the new instance.
  compile (Include _name) = do
    let name = pack _name
    return [text|
             this.constructor.prototype = Object.assign(this.constructor.prototype, Object.create($name.prototype));
             $name.call(this)
           |]

  -- TypeSignatures are ignored.
  compile (TypeSignature _ _ _)  = do return empty

  -- Interfaces are ignored.
  compile (Interface _ _ _)  = do return empty
  compile (Implement _)  = do return empty

  -- TypeAliases are ignored (we can't do anything without the aliased type).
  compile (TypeAlias _)  = do return empty

  -- Records are ignored (we can't do anything without the body).
  compile (Record _)  = do return empty

  compile _ = Nothing


instance Compilable Equation where
  -- | Compiles a Mulang Equation to a JS if statement.
  -- | The generated code is meant to be evaluated in the context of a container function, where the arguments variable
  -- | is available. The if condition will test the context's arguments against the expected patterns and either execute
  -- | and exit the context or avoid execution and continue context as is.
  -- | The context is expected to fail after all equations are tested unsuccessfully.
  compile (Equation argumentPatterns _body) = do
    let argumentCount = pack . show . length $ argumentPatterns
    let indexedPatterns = zip (map (pack . show) [0::Int ..]) argumentPatterns
    let conditions = Just [text| arguments.length === $argumentCount |] : map applyConditions indexedPatterns
    matchesPatterns <- fmap (intercalate (pack " && ")) . sequence $ conditions
    argumentNames   <- fmap (intercalate (pack "")) . sequence $ map nameAssignation indexedPatterns
    body            <- compile _body
    return [text| if($matchesPatterns){ $argumentNames $body } |]
    where

      -- Compiles a pattern and applies the resulting JS function with the pattern's position argument.
      applyConditions (n, pattern) = do
        cond <- compile pattern
        return [text| $cond(arguments[$n]) |]

      -- Converts a pair (Index, Mulang Pattern) to a JS code fragment that assigns arguments to the name the body uses
      -- to reference them.
      nameAssignation :: (Text, Pattern) -> Maybe Text
      nameAssignation (n, pattern) = case pattern of
        (VariablePattern _name) -> let name = pack _name in Just [text| var $name = arguments[$n]; |]
        (TypePattern _)         -> Just [text| |] --TODO: The variable name is missing. How do you identify the parameter in the body?
        (LiteralPattern _)      -> Just [text| |]
        WildcardPattern         -> Just [text| |]
        _                       -> Nothing -- TODO: Complete missing scenarios.


-- | Compiles a Mulang Pattern to a JS function that returns wheter it's argument matches the pattern or not.
-- | the arguments JS variable is used instead of parameters to avoid name collisions with user's code.
-- | A primitive function isInstanceOf is called for TypePatterns.
instance Compilable Pattern where
  compile (LiteralPattern _literal) = do
    let literal = pack . show $ _literal
    return [text| function(){ return arguments[0] === $literal } |]
  compile (TypePattern _typeName)   = do
    let typeName = pack _typeName
    return [text| function(){ return isInstanceOf(arguments[0], $typeName) } |]
  compile (VariablePattern _)       = return [text| function(){ return true } |]
  compile WildcardPattern           = return [text| function(){ return true } |]
  compile _                         = Nothing -- TODO: Complete missing scenarios.


-- | Compiles a Mulang EquationBody to a JS code fragment. The generated code is intended to be contained within an
-- | Equation and should only be evaluated once the pattern has been checked.
instance Compilable EquationBody where
  compile (UnguardedBody _result) = do
    result <- compile _result
    return [text| return $result |]

  compile (GuardedBody _cases) = do
    cases <- compileAll _cases " else "
    return [text| $cases throw new MuPatternMatchError() |]

type Guard = (Expression, Expression)
instance Compilable Guard where
  compile (_condition, _expression) = do
    condition <- compile _condition
    result <- compile _expression
    return [text| if($condition) { return $result } |]

type Catch = (Pattern, Expression)
instance Compilable Catch where
 compile (_pattern, _body) = compile $ Equation [_pattern] (UnguardedBody _body)