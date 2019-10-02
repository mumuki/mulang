# Supported inspections

The power of Mulang is grounded on more than 90 different kind of inspections

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
| `Delegates`                       | is a method, function or procedure declared AND called?
| `DiscardsExceptions`              | are exceptions discarded within an empty catch block?
| `DoesConsolePrint`                | is there any console-print-statement like `System.out.println`, `puts` or `console.log`?
| `DoesTypeTest`                    |
| `HasAssignmentReturn`             |
| `HasCodeDuplication`              | has the given code simple literal code duplication?
| `HasEmptyIfBranches`              | has the given code an empty if branch?
| `HasLongParameterList`            | does a given method/function/predicate take too many parameters?
| `HasMisspelledIdentifiers`        | an identifier is not a domain language dictionary's word and not part of its jargon
| `HasRedundantBooleanComparison`   |
| `HasRedundantIf`                  | can a combination of `if`s, `assignment`s and `return`s be replaced by a boolean expression?
| `HasRedundantLocalVariableReturn` |
| `HasRedundantRepeat`              | has the given code an unnecesary - 1 iteration - `repeat` statement?
| `HasTooShortIdentifiers`          | whether an identifier is too short and not part of domain language's jargon
| `HasUnreachableCode`              | is there unreachable code?
| `HasWrongCaseIdentifiers`         | whether an identifier does not match the domain language's case style
| `IsLongCode`                      | has the code long sequences of statements?
| `Raises`                          | is the given _exception type_ raised?
| `Rescues`                         | is the given _exception type_ rescued?
| `TypesAs`                         | is the given type used to type a variable?
| `TypesParameterAs`                | is a parameter typed as a given type?
| `TypesReturnAs`                   | is the given type used to type a return?
| `Uses`                            | is there any reference to the given element?
| `UsesArithmetic`                  | are arithmetic operators used?
| `UsesBooleanLogic`                | are logical operators used?
| `UsesConditional`                 |
| `UsesExceptionHandling`           | is any _exception_ handlded?
| `UsesExceptions`                  | is any _exception_ raised?
| `UsesFor`                         | is any kind of comprehension or indexed repetition used?
| `UsesIf`                          | is an `if` control structure used?
| `UsesLogic`                       | are boolean operators used?
| `UsesMath`                        | are artithmetic operators used?
| `UsesPrimitive`                   | is the given primitive operator used?
| `UsesPrint`                       | is a print statement used?
| `UsesType`                        | is the given typed used in a signature?


## Imperative Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DeclaresEnumeration`             | is a given enumeration declared?
| `DeclaresProcedure`               | is a given procedure declared?
| `UsesForEach`                     | is the procedural indexed repetition used?
| `UsesForLoop`                     | is a c-style for loop used?
| `UsesLoop`                        | are any of: repeat / for loop / foreach / while used?
| `UsesRepeat`                      |
| `UsesSwitch`                      |
| `UsesWhile`                       | is a `while` control structure used?

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
| `DoesNullTest`                    | is there a test agains a null value, like `if x == nil then puts 'is nil'`
| `HasTooManyMethods`               | does a given class/object/interface have too many methods?
| `Implements`                      | is the given interface implemented?
| `Includes`                        | is a given mixins included?
| `Inherits`                        | is a given class declared as superclass? - alias of `declaresSuperclass`
| `Instantiates`                    | is the given class instantiated?
| `OverridesEqualsOrHashButNotBoth` | does a given class override equals but not hash? or hash but not equals?
| `ReturnsNil`                      |
| `UsesDyamicPolymorphism`          | are there two or more methods definitions for some sent selector?
| `UsesDynamicMethodOverload`       | is there a class that defined two methods with different arity but with the same name?
| `UsesInheritance`                 | is any superclass explicitly declared?
| `UsesMixins`                      | is any mixins explicitly included?
| `UsesObjectComposition`           | is there a class that declares an attributes and sends a message to it?
| `UsesStaticMethodOverload`        | is there a class that defined two method signatures but with the same name?
| `UsesStaticPolymorphism`          | is there an interface with at least a method signature that is implemented by two or more classes and used in the code?
| `UsesTemplateMethod`              | is there a class that sends a message whose corresonding method is not declared?


## Functional Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `HasRedundantGuards`              |
| `HasRedundantLambda`              |
| `HasRedundantParameter`           |
| `UsesAnonymousVariable`           |
| `UsesComposition`                 |
| `UsesForComprehension`            | is the functional for/do/list comprehension used?
| `UsesGuards`                      |
| `UsesLambda`                      |
| `UsesYield`                       | is an expression yielded within a comprehension?

## Logic Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `DeclaresFact`                    | is a given logic fact declared?
| `DeclaresPredicate`               | is a given rule o fact declared?
| `DeclaresRule`                    | is a given logic rule declared?
| `HasRedundantReduction`           | is a is-operator used to unify individuals that don't require a reduction, like `X is 4`
| `UsesCut`                         | is the logic `!` consult used?
| `UsesFail`                        | is the logic `fail` consult used?
| `UsesFindall`                     | is the logic `findall` consult used?
| `UsesForall`                      | is the logic `forall` consult used?
| `UsesNot`                         |
| `UsesUnificationOperator`         | is the logic unification operator `=` used?
