# Supported inspections

The power of Mulang is grounded on more than 90 different kind of inspections

## Generic Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `assigns`                         | the given variable or attribute assigned?
| `assignsMatching`                 | the given variable or attribute assigned matching the given value?
| `calls`                           | is the given method, function or procedure called?
| `callsMatching`                   | is the given method, function or procedure called matching the given arguments?
| `declares`                        | is the given element declared?
| `declaresComputation`             | that is, does the given computation  - method, predicate, function, etc - exist?
| `declaresComputationWithArity`    | that is, does the given computation have the exact given arity?
| `declaresEntryPoint`              | is there a program entry point, like a `main` procedure?
| `declaresFunction`                | is a given function declared?
| `declaresFunctionMatching`        | is a given function declared matching the given body expressions?
| `declaresRecursively`             | is a given computation declared using recusion?
| `declaresTypeAlias`               | is a given type synonym declared?
| `declaresTypeSignature`           | is a given computation type signature declared?
| `declaresVariable`                | is a given local or global variable declared?
| `declaresVariableMatching`        | is a given local or global variable declared matching the given expression?
| `delegates`                       | is a method, function or procedure declared AND called?
| `discardsExceptions`              | are exceptions discarded within an empty catch block?
| `doesConsolePrint`                | is there any console-print-statement like `System.out.println`, `puts` or `console.log`?
| `doesTypeTest`                    |
| `hasAssignmentReturn`             |
| `hasCodeDuplication`              | has the given code simple literal code duplication?
| `hasEmptyIfBranches`              | has the given code an empty if branch?
| `hasLongParameterList`            | does a given method/function/predicate take too many parameters?
| `hasMisspelledIdentifiers`        | an identifier is not a domain language dictionary's word and not part of its jargon
| `hasRedundantBooleanComparison`   |
| `hasRedundantIf`                  | can a combination of `if`s, `assignment`s and `return`s be replaced by a boolean expression?
| `hasRedundantLocalVariableReturn` |
| `hasRedundantRepeat`              | has the given code an unnecesary - 1 iteration - `repeat` statement?
| `hasTooShortIdentifiers`          | whether an identifier is too short and not part of domain language's jargon
| `hasUnreachableCode`              | is there unreachable code?
| `hasWrongCaseIdentifiers`         | whether an identifier does not match the domain language's case style
| `isLongCode`                      | has the code long sequences of statements?
| `raises`                          | is the given _exception type_ raised?
| `rescues`                         | is the given _exception type_ rescued?
| `returnsMatching`                 | is a return used matching the given value?
| `typesAs`                         | is the given type used to type a variable?
| `typesParameterAs`                | is a parameter typed as a given type?
| `typesReturnAs`                   | is the given type used to type a return?
| `uses`                            | is there any reference to the given element?
| `usesArithmetic`                  | are arithmetic operators used?
| `usesBooleanLogic`                | are logical operators used?
| `usesConditional`                 |
| `usesExceptionHandling`           | is any _exception_ handlded?
| `usesExceptions`                  | is any _exception_ raised?
| `usesFor`                         | is any kind of comprehension or indexed repetition used?
| `usesIf`                          | is an `if` control structure used?
| `usesLogic`                       | are boolean operators used?
| `usesMath`                        | are artithmetic operators used?
| `usesPrimitive`                   | is the given primitive operator used?
| `usesPrint`                       | is a print statement used?
| `usesType`                        | is the given typed used in a signature?


## Imperative Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `declaresEnumeration`             | is a given enumeration declared?
| `declaresProcedure`               | is a given procedure declared?
| `declaresProcedureMatching`       | is a given procedure declared matching the given body expressions?
| `usesForEach`                     | is the procedural indexed repetition used?
| `usesForLoop`                     | is a c-style for loop used?
| `usesLoop`                        | are any of: repeat / for loop / foreach / while used?
| `usesRepeat`                      |
| `usesSwitch`                      |
| `usesWhile`                       | is a `while` control structure used?

## Object Oriented Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `declaresAttribute`               | is a given attribute declared?
| `declaresClass`                   | is a given class declared?
| `declaresClassMatching`           | is a given class declared matching the given body expressions?
| `declaresInterface`               | is a given interface declared?
| `declaresInterfaceMatching`       | is a given interface declared matching the given body expressions?
| `declaresMethod`                  | is a given method declared?
| `declaresMethodMatching`          | is a given method declared matching the given body expressions?
| `declaresObject`                  | is a given named object declared?
| `declaresObjectMatching`          | is a given named object declared matching the given body expressions?
| `declaresPrimitive`               | Is the given primitive operator overriden?
| `declaresSuperclass`              | is a given class declared as superclass?
| `doesNullTest`                    | is there a test agains a null value, like `if x == nil then puts 'is nil'`
| `hasTooManyMethods`               | does a given class/object/interface have too many methods?
| `implements`                      | is the given interface implemented?
| `includes`                        | is a given mixins included?
| `inherits`                        | is a given class declared as superclass? - alias of `declaresSuperclass`
| `instantiates`                    | is the given class instantiated?
| `overridesEqualsOrHashButNotBoth` | does a given class override equals but not hash? or hash but not equals?
| `returnsNil`                      |
| `usesDyamicPolymorphism`          | are there two or more methods definitions for some sent selector?
| `usesDynamicMethodOverload`       | is there a class that defined two methods with different arity but with the same name?
| `usesInheritance`                 | is any superclass explicitly declared?
| `usesMixins`                      | is any mixins explicitly included?
| `usesObjectComposition`           | is there a class that declares an attributes and sends a message to it?
| `usesStaticMethodOverload`        | is there a class that defined two method signatures but with the same name?
| `usesStaticPolymorphism`          | is there an interface with at least a method signature that is implemented by two or more classes and used in the code?
| `usesTemplateMethod`              | is there a class that sends a message whose corresonding method is not declared?


## Functional Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `hasRedundantGuards`              |
| `hasRedundantLambda`              |
| `hasRedundantParameter`           |
| `usesAnonymousVariable`           |
| `usesComposition`                 |
| `usesForComprehension`            | is the functional for/do/list comprehension used?
| `usesGuards`                      |
| `usesLambda`                      |
| `usesPatternMatching`             |
| `usesYield`                       | is an expression yielded within a comprehension?

## Logic Inspections

| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `declaresFact`                    | is a given logic fact declared?
| `declaresPredicate`               | is a given rule o fact declared?
| `declaresRule`                    | is a given logic rule declared?
| `hasRedundantReduction`           | is a is-operator used to unify individuals that don't require a reduction, like `X is 4`
| `usesCut`                         | is the logic `!` consult used?
| `usesFail`                        | is the logic `fail` consult used?
| `usesFindall`                     | is the logic `findall` consult used?
| `usesForall`                      | is the logic `forall` consult used?
| `usesNot`                         |
| `usesUnificationOperator`         | is the logic unification operator `=` used?
