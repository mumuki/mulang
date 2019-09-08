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


## Object Oriented Inspections


| Inspection                        | Meaning
|-----------------------------------|------------------------------------------------------
| `declaresAttribute`               |  object oriented   | is a given attribute declared?
| `declaresClass`                   |  object oriented   | is a given class declared?
| `declaresClassMatching`           |  object oriented   | is a given class declared matching the given body expressions?
| `declaresEnumeration`             |  imperative        | is a given enumeration declared?
| `declaresFact`                    |  logic             | is a given logic fact declared?
| `declaresFunction`                |  functional/imperative | is a given function declared?
| `declaresFunctionMatching`        |  functional/imperative | is a given function declared matching the given body expressions?
| `declaresInterface`               |  object oriented   | is a given interface declared?
| `declaresInterfaceMatching`       |  object oriented   | is a given interface declared matching the given body expressions?
| `declaresMethod`                  |  object oriented   | is a given method declared?
| `declaresMethodMatching`          |  object oriented   | is a given method declared matching the given body expressions?
| `declaresObject`                  |  object oriented   | is a given named object declared?
| `declaresObjectMatching`          |  object oriented   | is a given named object declared matching the given body expressions?
| `declaresPredicate`               |  logic             | is a given rule o fact declared?
| `declaresPrimitive`               |  object oriented   | Is the given primitive operator overriden?
| `declaresProcedure`               |  imperative        | is a given procedure declared?
| `declaresProcedureMatching`       |  imperative        | is a given procedure declared matching the given body expressions?
| `declaresRecursively`             |  any               | is a given computation declared using recusion?
| `declaresRule`                    |  logic             | is a given logic rule declared?
| `declaresSuperclass`              |  object oriented   | is a given class declared as superclass?
| `declaresTypeAlias`               |  any               | is a given type synonym declared?
| `declaresTypeSignature`           |  any               | is a given computation type signature declared?
| `declaresVariable`                |  any               | is a given local or global variable declared?
| `declaresVariableMatching`        |  any               | is a given local or global variable declared matching the given expression?
| `delegates`                       |  delegates         | is a method, function or procedure declared AND called?
| `discardsExceptions`              |  any               | are exceptions discarded within an empty catch block?
| `doesConsolePrint`                |  any               | is there any console-print-statement like `System.out.println`, `puts` or `console.log`?
| `doesNullTest`                    |  object oriented   | is there a test agains a null value, like `if x == nil then puts 'is nil'`
| `doesTypeTest`                    |
| `hasAssignmentReturn`             |
| `hasCodeDuplication`              |  any               | has the given code simple literal code duplication?
| `hasEmptyIfBranches`              |  any               | has the given code an empty if branch?
| `hasLongParameterList`            |  any               | does a given method/function/predicate take too many parameters?
| `hasMisspelledIdentifiers`        |  any               | an identifier is not a domain language dictionary's word and not part of its jargon
| `hasRedundantBooleanComparison`   |
| `hasRedundantGuards`              |
| `hasRedundantIf`                  |  any               | can a combination of `if`s, `assignment`s and `return`s be replaced by a boolean expression?
| `hasRedundantLambda`              |
| `hasRedundantLocalVariableReturn` |
| `hasRedundantParameter`           |
| `hasRedundantReduction`           |  logic             | is a is-operator used to unify individuals that don't require a reduction, like `X is 4`
| `hasTooManyMethods`               |  object oriented   | does a given class/object/interface have too many methods?
| `hasTooShortIdentifiers`          |  any               | whether an identifier is too short and not part of domain language's jargon
| `hasUnreachableCode`              |  any               | is there unreachable code?
| `hasWrongCaseIdentifiers`         |  any               | whether an identifier does not match the domain language's case style
| `implements`                      |  object oriented   | is the given interface implemented?
| `includes`                        |  object oriented   | is a given mixins included?
| `inherits`                        |  object oriented   | is a given class declared as superclass? - alias of `declaresSuperclass`
| `instantiates`                    |  object oriented   | is the given class instantiated?
| `isLongCode`                      |  any               | has the code long sequences of statements?
| `overridesEqualsOrHashButNotBoth` |  object oriented   | does a given class override equals but not hash? or hash but not equals?
| `raises`                          |  any               | is the given _exception type_ raised?
| `rescues`                         |  any               | is the given _exception type_ rescued?
| `returnsMatching`                 |  any               | is a return used matching the given value?
| `returnsNil`                      |
| `typesAs`                         |  any               | is the given type used to type a variable?
| `typesParameterAs`                |  any               | is a parameter typed as a given type?
| `typesReturnAs`                   |  any               | is the given type used to type a return?
| `uses`                            |  any               | is there any reference to the given element?
| `usesAnonymousVariable`           |
| `usesArithmetic`                  |  any               | are arithmetic operators used?
| `usesBooleanLogic`                |  any               | are logical operators used?
| `usesComposition`                 |
| `usesConditional`                 |
| `usesCut`                         |  logic             | is the logic `!` consult used?
| `usesDyamicPolymorphism`          |  object oriented   | are there two or more methods definitions for some sent selector?
| `usesDynamicMethodOverload`       |  object oriented   | is there a class that defined two methods with different arity but with the same name?
| `usesExceptionHandling`           |  any               | is any _exception_ handlded?
| `usesExceptions`                  |  any               | is any _exception_ raised?
| `usesFail`                        |  logic             | is the logic `fail` consult used?
| `usesFindall`                     |  logic             | is the logic `findall` consult used?
| `usesFor`                         |  any               | is any kind of comprehension or indexed repetition used?
| `usesForall`                      |  logic             | is the logic `forall` consult used?
| `usesForComprehension`            |  functional        | is the functional for/do/list comprehension used?
| `usesForEach`                     |  procedural        | is the procedural indexed repetition used?
| `usesForLoop`                     |  procedural        | is a c-style for loop used?
| `usesGuards`                      |
| `usesIf`                          |  any               | is an `if` control structure used?
| `usesInheritance`                 |  object oriented   | is any superclass explicitly declared?
| `usesLambda`                      |
| `usesLogic`                       |  any               | are boolean operators used?
| `usesLoop`                        |  procedural        | are any of: repeat / for loop / foreach / while used?
| `usesMath`                        |  any               | are artithmetic operators used?
| `usesMixins`                      |  object oriented   | is any mixins explicitly included?
| `usesNot`                         |
| `usesObjectComposition`           |  object oriented   | is there a class that declares an attributes and sends a message to it?
| `usesPatternMatching`             |
| `usesPrimitive`                   |                    | is the given primitive operator used?
| `usesPrint`                       |  any               | is a print statement used?
| `usesRepeat`                      |
| `usesStaticMethodOverload`        |  object oriented   | is there a class that defined two method signatures but with the same name?
| `usesStaticPolymorphism`          |  object oriented   | is there an interface with at least a method signature that is implemented by two or more classes and used in the code?
| `usesSwitch`                      |
| `usesTemplateMethod`              |  object oriented   | is there a class that sends a message whose corresonding method is not declared?
| `usesType`                        |  any               | is the given typed used in a signature?
| `usesUnificationOperator`         |  logic             | is the logic unification operator `=` used?
| `usesWhile`                       |  imperative        | is a `while` control structure used?
| `usesYield`                       |  functional        | is an expression yielded within a comprehension?
