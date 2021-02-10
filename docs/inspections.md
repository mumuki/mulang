# Supported inspections

The power of Mulang is grounded on more than 120 different kind of inspections

## Generic Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `Assigns`                         | the given variable or attribute assigned?
| `Calls`                           | is the given method, function or procedure called?
| `Declares`                        | is the given element declared?
| `DeclaresComputation`             | does the given computation  - method, predicate, function, etc - exist?
| `DeclaresComputationWithArity0`   | does the given computation have no arguments?
| `DeclaresComputationWithArity1`   | does the given computation have 1 argument?
| `DeclaresComputationWithArity2`   | does the given computation have 2 arguments?
| `DeclaresComputationWithArity3`   | does the given computation have 3 arguments?
| `DeclaresComputationWithArity4`   | does the given computation have 4 arguments?
| `DeclaresComputationWithArity5`   | does the given computation have 5 arguments?
| `DeclaresEntryPoint`              | is there a program entry point, like a `main` procedure?
| `DeclaresFunction`                | is a given function declared?
| `DeclaresRecursively`             | is a given computation declared using recusion?
| `DeclaresTypeAlias`               | is a given type synonym declared?
| `DeclaresTypeSignature`           | is a given computation type signature declared?
| `DeclaresVariable`                | is a given local or global variable declared?
| `Delegates`                       | is a non-empty method, function or procedure declared *and* called?
| `Raises`                          | is the given _exception type_ raised?
| `Rescues`                         | is the given _exception type_ rescued?
| `SubordinatesDeclarationsTo`      | are all the declarations in the code called from the given declaration?
| `SubordinatesDeclarationsToEntryPoint` | are all the declarations in the code called from an entry point?
| `TypesAs`                         | is the given type used to type a variable?
| `TypesParameterAs`                | is a parameter typed as a given type?
| `TypesReturnAs`                   | is the given type used to type a return?
| `Uses`                            | is there any reference to the given element?
| `UsesArithmetic`                  | are arithmetic operators used?
| `UsesConditional`                 |
| `UsesExceptionHandling`           | is any _exception_ handlded?
| `UsesExceptions`                  | is any _exception_ raised?
| `UsesFor`                         | is any kind of comprehension or indexed repetition used?
| `UsesIf`                          | is an `if` control structure used?
| `UsesLogic`                       | are boolean operators used?
| `UsesMath`                        | are artithmetic operators used?
| `UsesPrint`                       | is a print statement used?
| `UsesType`                        | is the given typed used in a signature?


### Code Smells

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DiscardsExceptions`              | are exceptions discarded within an empty catch block?
| `DoesConsolePrint`                | is there any console-print-statement like `System.out.println`, `puts` or `console.log`?
| `HasCodeDuplication`              | has the given code simple literal code duplication?
| `HasDeclarationTypos`             | is an identifier *not* declared but a very similar one declared instead?
| `HasEmptyIfBranches`              | has the given code an empty `if` branch?
| `HasEqualIfBranches`              | are both branches of an `if` equal?
| `HasLongParameterList`            | does a given method/function/predicate take too many parameters?
| `HasMisspelledIdentifiers`        | an identifier is not a domain language dictionary's word and not part of its jargon
| `HasRedundantBooleanComparison`   |
| `HasRedundantIf`                  | can a combination of `if`s, `assignment`s and `return`s be replaced by a boolean expression?
| `HasRedundantLocalVariableReturn` | does a callable declare and return a variable just after declaring it?
| `HasTooShortIdentifiers`          | whether an identifier is too short and not part of domain language's jargon
| `HasUnreachableCode`              | is there unreachable code?
| `HasUsageTypos`                   | is an identifier *not* called but a very similar one called instead?
| `HasWrongCaseIdentifiers`         | whether an identifier does not match the domain language's case style
| `IsLongCode`                      | has the code long sequences of statements?
| `ShouldInvertIfCondition`         | has the given code an `if` with an empty `then` but a non-empty `else`?
| `ShouldUseStrictComparators`      | does the given use a non-strict comparator like `==` in JavaScript?
| `JavaScript#UsesVarInsteadOfLet`  | ⚠️ **JavaScript-specific** does the code use `var` instead of `let`?

## Primitive Operator Inspections

> 👀 See also [operators section in AST Specs](../astspec/#primitive-operators).
>
> ⚠️ Please notice that the operators inspections are the preferred and most reliable way of checking
> usage of language primitives. For example, prefer `UsesPlus` over `Uses:+`

| Inspection                       | Meaning
|----------------------------------|-----------------------
| `UsesAbsolute`                   | is the numeric `abs`-like absolute operator used?
| `UsesAllSatisfy`                 | is the collection `all`-like / `every`-like operator used?
| `UsesAnd`                        | is the `&&`-like and operator used?
| `UsesAnySatisfy`                 | is the collection `any`-like / `some`-like operator used?
| `UsesBackwardComposition`        | is the `.`-like functional backward composition operator used?
| `UsesBitwiseAnd`                 | is the bit-level `&`-like and operator used?
| `UsesBitwiseLeftShift`           | is the bit-level left `<<`-like shift operator used?
| `UsesBitwiseOr`                  | is the bit-level `|`-like or operator used?
| `UsesBitwiseRightShift`          | is the bit-level right `>>`-like shift operator used?
| `UsesBitwiseXor`                 | is the bit-level `^`-like xor operator used?
| `UsesCeil`                       | is the numeric `ceil`-like ceiling operator used?
| `UsesCollect`                    | is the collection `map`-like operator used?
| `UsesCount`                      | is the collection `count`-like operator used?
| `UsesDetect`                     | is the collection `find`-like search operator used?
| `UsesDetectMax`                  | is the collection `max`-like maximum operator used?
| `UsesDetectMin`                  | is the collection `min`-like minumum operator used?
| `UsesDivide`                     | is the numeric `/` operator used?
| `UsesEqual`                      | is the `===`-like equal operator used?
| `UsesFlatten`                    | is the collection `flatten`-like operator used?
| `UsesFloor`                      | is the numeric `ceil`-like floor operator used?
| `UsesForwardComposition`         | is the `>>`-like functional forward composition operator used?
| `UsesGather`                     | is the collection `flatmap`-like operator used?
| `UsesGetAt`                      | is the collection `[]`-like operator used?
| `UsesGreatherOrEqualThan`        | is the `>=` operator used?
| `UsesGreatherThan`               | is the `>` operator used?
| `UsesHash`                       | is the `hashcode` operator used?
| `UsesInject`                     | is the collection `reduce`-like / `fold`-like operator used?
| `UsesLessOrEqualThan`            | is the `<=` operator used?
| `UsesLessThan`                   | is the `<` operator used?
| `UsesMax`                        | is the `max`-like maximum value binary operator used?
| `UsesMin`                        | is the `min`-like minimum value binary operator used?
| `UsesMinus`                      | is the numeric `-` operator used?
| `UsesModulo`                     | is the numeric `%-like` modulo operator used?
| `UsesMultiply`                   | is the numeric `*` operator used?
| `UsesNegation`                   | is the `!`-like not operator used?
| `UsesNotEqual`                   | is the `!==`-like distinct operator used?
| `UsesNotSame`                    | is the not reference-identical operator used?
| `UsesNotSimilar`                 | is the not equal-ignoring-type operator used?
| `UsesOr`                         | is the `||`-like or operator used?
| `UsesOtherwise`                  | is the guard's otherwise operator used?
| `UsesPlus`                       | is the numeric `+` operator used?
| `UsesPush`                       | is the collection `insertAtEnd`-like operator used?
| `UsesRound`                      | is the numeric `round`-like round operator used?
| `UsesSame`                       | is the reference-identical operator used?
| `UsesSelect`                     | is the collection `filter`-like operator used?
| `UsesSetAt`                      | is the collection `[]=`-like operator used?
| `UsesSimilar`                    | is the equal-ignoring-type operator used?
| `UsesSize`                       | is the collection `length`-like size operator used?


## Imperative Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DeclaresEnumeration`             | is a given enumeration declared?
| `DeclaresProcedure`               | is a given procedure declared?
| `UsesForEach`                     | is the procedural indexed repetition used?
| `UsesForLoop`                     | is a c-style for loop used?
| `UsesLoop`                        | are any of: repeat / for loop / foreach / while used?
| `UsesRepeat`                      |
| `UsesSwitch`                      | is a `switch` control structure used?
| `UsesWhile`                       | is a `while` control structure used?

### Code Smells

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `HasAssignmentCondition`          | is the code evaluating the result of an assignment where a boolean condition is expected?
| `HasAssignmentReturn`             | is the code returning the result of an assignment?
| `HasEmptyRepeat`                  | has the given code a `repeat` with empty body?
| `HasRedundantRepeat`              | has the given code an unnecesary - 1 iteration - `repeat` statement?

## Object Oriented Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DeclaresAttribute`               | is a given attribute declared?
| `DeclaresClass`                   | is a given class declared?
| `DeclaresInterface`               | is a given interface declared?
| `DeclaresMethod`                  | is a given method declared?
| `DeclaresObject`                  | is a given named object declared?
| `DeclaresPrimitive`               | Is the given primitive operator overriden?
| `DeclaresSuperclass`              | is a given class declared as superclass?
| `Implements`                      | is the given interface implemented?
| `Includes`                        | is a given mixins included?
| `Inherits`                        | is a given class declared as superclass? - alias of `declaresSuperclass`
| `Instantiates`                    | is the given class instantiated?
| `UsesDynamicPolymorphism`          | are there two or more methods definitions for some sent selector?
| `UsesDynamicMethodOverload`       | is there a class that defined two methods with different arity but with the same name?
| `UsesInheritance`                 | is any superclass explicitly declared?
| `UsesMixins`                      | is any mixins explicitly included?
| `UsesObjectComposition`           | is there a class that declares an attributes and sends a message to it?
| `UsesStaticMethodOverload`        | is there a class that defined two method signatures but with the same name?
| `UsesStaticPolymorphism`          | is there an interface with at least a method signature that is implemented by two or more classes and used in the code?
| `UsesTemplateMethod`              | is there a class that sends a message whose corresonding method is not declared?



### Code Smells

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DoesNilTest`                     | is there a test agains a null value, like `if x == nil then puts 'is nil'`
| `DoesTypeTest`                    | are there any tests against literal strings?
| `HasTooManyMethods`               | does a given class/object/interface have too many methods?
| `OverridesEqualOrHashButNotBoth` | does a given class override equals but not hash? or hash but not equals?
| `ReturnsNil`                      |
| `UsesNamedSelfReference`          | does an object reference itself by its name instead of using `self`?

## Functional Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `UsesAnonymousVariable`           |
| `UsesComposition`                 |
| `UsesForComprehension`            | is the functional for/do/list comprehension used?
| `UsesGuards`                      |
| `UsesLambda`                      |
| `UsesYield`                       | is an expression yielded within a comprehension?

### Code Smells

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `HasRedundantGuards`              |
| `HasRedundantLambda`              |
| `HasRedundantParameter`           |
| `ShouldUseOtherwise`              |

## Logic Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DeclaresFact`                    | is a given logic fact declared?
| `DeclaresPredicate`               | is a given rule o fact declared?
| `DeclaresRule`                    | is a given logic rule declared?
| `UsesFindall`                     | is the logic `findall` consult used?
| `UsesForall`                      | is the logic `forall` consult used?
| `UsesNot`                         |


### Code Smells

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `HasRedundantReduction`           | is a is-operator used to unify individuals that don't require a reduction, like `X is 4`
| `UsesCut`                         | is the logic `!` consult used?
| `UsesFail`                        | is the logic `fail` consult used?
| `UsesUnificationOperator`         | is the logic unification operator `=` used?
