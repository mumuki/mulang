[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

Mulang
======
> A universal, multi-language, multi-paradigm code analyzer

Mulang is three different  - but thighly related - things:

  * an intermediate language, known as the [Mulang AST](#mulang-ast-spec);
  * a [command line tool](#command-line-tool) for analysing the Mulang AST and [some popular languages](#supported-languages) by transforming to it. This tool is distributed as both a `linux-amd64` binary and a JavaScript package. See [downloads section](https://github.com/mumuki/mulang/releases).
  * a Haskell composable [combinators library]((#combinators-crash-course)) for analysing the Mulang AST;

# Table of contents

<!-- toc -->

- [Combinators Crash course](#combinators-crash-course)
  * [Inspections](#inspections)
  * [Inspection Combinators](#inspection-combinators)
  * [Identifier predicates](#identifier-predicates)
  * [Detections](#detections)
- [Supported inspections](#supported-inspections)
- [Supported languages](#supported-languages)
- [Command Line Tool](#command-line-tool)
  * [The expectations DSL](#the-expectations-dsl)
  * [Examples](#examples)
    + [With intransitive expectations](#with-intransitive-expectations)
    + [With unscoped expectations](#with-unscoped-expectations)
    + [With signature analysis](#with-signature-analysis)
    + [With broken input](#with-broken-input)
    + [With AST as input](#with-ast-as-input)
    + [With Smell Analysis, by inclusion](#with-smell-analysis-by-inclusion)
    + [With Smell Analysis, by exclusion](#with-smell-analysis-by-exclusion)
    + [With expressiveness smells](#with-expressiveness-smells)
    + [With Intermediate Language Generation](#with-intermediate-language-generation)
  * [With test running](#with-test-running)
- [Mulang AST spec](#mulang-ast-spec)
  * [Expressions](#expressions)
    + [`Record`](#record)
      - [Syntax](#syntax)
      - [C Example](#c-example)
      - [Caveats](#caveats)
    + [`TypeAlias`, `TypeSignature` and `TypeCast`](#typealias-typesignature-and-typecast)
    + [`EntryPoint`](#entrypoint)
      - [Syntax](#syntax-1)
      - [Java Example](#java-example)
    + [`Function`](#function)
      - [Syntax](#syntax-2)
      - [Example](#example)
    + [`Procedure`](#procedure)
      - [Syntax](#syntax-3)
    + [`Method`](#method)
      - [Syntax](#syntax-4)
      - [Ruby Example](#ruby-example)
      - [Java Example](#java-example-1)
    + [`EqualMethod` and `HashMethod`](#equalmethod-and-hashmethod)
      - [Syntax](#syntax-5)
      - [Ruby Example](#ruby-example-1)
    + [`Variable`](#variable)
      - [Syntax](#syntax-6)
      - [Example](#example-1)
    + [`Assignment`](#assignment)
      - [Syntax](#syntax-7)
      - [Example](#example-2)
    + [`Attribute`](#attribute)
      - [Syntax](#syntax-8)
      - [Example](#example-3)
    + [`Object`](#object)
      - [Syntax](#syntax-9)
      - [Example](#example-4)
    + [`Class`](#class)
      - [Syntax](#syntax-10)
      - [Ruby Example](#ruby-example-2)
      - [Java Examples](#java-examples)
    + [`Enumeration`](#enumeration)
      - [Syntax](#syntax-11)
      - [Java Example](#java-example-2)
    + [`Interface`](#interface)
      - [Syntax](#syntax-12)
      - [Java Example](#java-example-3)
    + [`Rule`](#rule)
      - [Syntax](#syntax-13)
      - [Example](#example-5)
    + [`Fact`](#fact)
      - [Syntax](#syntax-14)
      - [Example](#example-6)
    + [`Exist`](#exist)
      - [Syntax](#syntax-15)
      - [Example](#example-7)
    + [`Not`](#not)
      - [Syntax](#syntax-16)
      - [Example](#example-8)
    + [`Findall`](#findall)
      - [Syntax](#syntax-17)
      - [Example](#example-9)
    + [`Forall`](#forall)
      - [Syntax](#syntax-18)
      - [Example](#example-10)
    + [`Reference`](#reference)
      - [Syntax](#syntax-19)
      - [Example](#example-11)
    + [`Application`](#application)
      - [Syntax](#syntax-20)
      - [Example](#example-12)
    + [`Send`](#send)
      - [Syntax](#syntax-21)
      - [Ruby Example](#ruby-example-3)
    + [`New`](#new)
      - [Syntax](#syntax-22)
      - [Example](#example-13)
    + [`Implement`](#implement)
      - [Syntax](#syntax-23)
      - [Example](#example-14)
    + [`Include`](#include)
      - [Syntax](#syntax-24)
      - [Example](#example-15)
    + [`If`](#if)
      - [Syntax](#syntax-25)
    + [`Lambda`](#lambda)
      - [Syntax](#syntax-26)
    + [`Return`](#return)
      - [Syntax](#syntax-27)
    + [`While`](#while)
      - [Syntax](#syntax-28)
      - [Example](#example-16)
    + [`Repeat`](#repeat)
      - [Syntax](#syntax-29)
      - [Example](#example-17)
    + [`Match`](#match)
      - [Syntax](#syntax-30)
    + [`Switch`](#switch)
      - [Syntax](#syntax-31)
    + [`Try`](#try)
      - [Syntax](#syntax-32)
      - [Example](#example-18)
    + [`Raise`](#raise)
      - [Syntax](#syntax-33)
      - [Example](#example-19)
    + [`Print`](#print)
      - [Syntax](#syntax-34)
      - [Ruby Example](#ruby-example-4)
    + [`For`](#for)
      - [Syntax](#syntax-35)
      - [Haskell Example](#haskell-example)
      - [Java Example](#java-example-4)
    + [`ForLoop`](#forloop)
      - [Syntax](#syntax-36)
      - [Example](#example-20)
    + [`Sequence`](#sequence)
      - [Syntax](#syntax-37)
      - [Example](#example-21)
    + [`Other`](#other)
      - [Syntax](#syntax-38)
    + [`Equal` and `NotEqual`](#equal-and-notequal)
      - [Syntax](#syntax-39)
    + [`Self`](#self)
      - [Syntax](#syntax-40)
    + [`None`](#none)
      - [Syntax](#syntax-41)
    + [`MuNil`](#munil)
      - [Syntax](#syntax-42)
    + [`MuObject`](#muobject)
      - [Syntax](#syntax-43)
      - [JavaScript Example](#javascript-example)
    + [`MuNumber`, `MuBool`, `MuString`, `MuSymbol` and `MuChar`](#munumber-mubool-mustring-musymbol-and-muchar)
      - [Syntax](#syntax-44)
      - [Ruby Example](#ruby-example-5)
    + [`MuTuple` and `MuList`](#mutuple-and-mulist)
      - [Syntax](#syntax-45)
    + [`TestGroup`, `Test` and `Assert`](#testgroup-test-and-assert)
      - [Syntax](#syntax-46)
      - [Javascript Example](#javascript-example)
      - [Python Example](#python-example)
  * [Assertion](#assertion)
      - [Syntax](#syntax-47)
      - [Javascript Examples](#javascript-examples)
  * [Patterns](#patterns)
    + [`VariablePattern`](#variablepattern)
      - [Syntax](#syntax-48)
      - [JavaScript Example](#javascript-example-1)
    + [`LiteralPattern`](#literalpattern)
      - [Syntax](#syntax-49)
      - [Example](#example-22)
    + [`InfixApplicationPattern`](#infixapplicationpattern)
      - [Syntax](#syntax-50)
        * [Caveats](#caveats-1)
    + [`ApplicationPattern`](#applicationpattern)
      - [Syntax](#syntax-51)
      - [Example](#example-23)
    + [`TuplePattern`](#tuplepattern)
      - [Syntax](#syntax-52)
      - [Example](#example-24)
    + [`ListPattern`](#listpattern)
      - [Syntax](#syntax-53)
      - [Example](#example-25)
    + [`FunctorPattern`](#functorpattern)
      - [Syntax](#syntax-54)
      - [Example](#example-26)
    + [`AsPattern`](#aspattern)
      - [Syntax](#syntax-55)
      - [Example](#example-27)
    + [`TypePattern`](#typepattern)
      - [Syntax](#syntax-56)
      - [Example](#example-28)
    + [`WildcardPattern`](#wildcardpattern)
      - [Syntax](#syntax-57)
    + [`UnionPattern`](#unionpattern)
      - [Syntax](#syntax-58)
    + [`OtherPattern`](#otherpattern)
      - [Syntax](#syntax-59)
  * [Types](#types)
    + [`TypeAlias`](#typealias)
      - [Syntax](#syntax-60)
      - [Haskell Example](#haskell-example-1)
    + [TypeSignature](#typesignature)
      - [Syntax](#syntax-61)
      - [Haskell Examples](#haskell-examples)
      - [Java Examples](#java-examples-1)
    + [`TypeCast`](#typecast)
      - [Syntax](#syntax-62)
      - [Haskell Examples](#haskell-examples-1)
      - [Java Examples](#java-examples-2)
        * [Caveats](#caveats-2)
- [Code Execution](#code-execution)
    + [Examples](#examples-1)
- [Building mulang from source](#building-mulang-from-source)
  * [Setup](#setup)
  * [Installing and creating an executable](#installing-and-creating-an-executable)
  * [Running tests](#running-tests)
  * [Watching changes](#watching-changes)
  * [Loading mulang in the REPL](#loading-mulang-in-the-repl)
- [Ruby wrapper](#ruby-wrapper)
- [JavaScript library](#javascript-library)
- [Tagging and releasing](#tagging-and-releasing)
- [Contributors](#contributors)

<!-- tocstop -->

# Combinators Crash course

Better than explaining what Mulang is, let's see what can do it for you.

## Inspections

Let's suppose we have the following JS code...

```javascript
var aPlace = buenosAires;
var aBird = {position: aPlace, weight: 20};
```

...and we want to recognize some code patterns on it. We will first load the expression into Mulang:

```
$ ghci
> :m Language.Mulang.All
> let e = js "var aPlace = buenosAires; var aBird = {position: aPlace, weight: 20};"
```

Now the magic begins. We want to know if the code expression _uses_ - that is, contains any reference to - a given _identifier_. Such identifier could be a variable, function, or anything that has a name:

```haskell
> uses (named "buenosAires") e
True -- because of the reference in `...aPlace = buenosAires...`
> uses (named "rosario") e
False -- no reference to the identifier `rosario` is found on the code
```

`uses (named "buenosAires")` is our first _inspection_: a function that takes a Mulang AST and answers a boolan question about it. That _seems_ easy, but just in case you are wondering: no, Mulang doesn't perform a `string.contains` or something like that :stuck_out_tongue: :

```haskell
> uses (named "buenos") e
False -- no reference to the identifier `buenos` is found on the code
```

## Inspection Combinators

So let's ask something more interesting - does `aPlace` _use_ the identifier `buenosAires`?

```haskell
> scoped (uses (named "buenosAires")) "aPlace"  e
True -- again, because of the the reference in `...aPlace = buenosAires...`
> scoped (uses (named "buenosAires")) "aBird"  e
False -- because `...aBird = {position: aPlace, weight: 20}...` does not reference `buenosAires` directly...
```

Here we have used the our first _inspection combinator_, a function that takes an inspection - `uses (named "buenosAires")` - and returns a new one that is more powerful. In this case, `scoped` is capable of restricting the analysis to the given _context_ - the `aPlace` identifier.

Let's tray again: does `"aPlace"` it _use_ `rosario`?

```haskell
> scoped (uses (named "rosario")) "aPlace"  e
False
```

What about the object `aBird`? Does it use `aPlace` or `rosario`?

```haskell
> scoped (uses (named "aPlace")) "aBird"  e
True
> scoped (uses (named "rosario")) "aBird"  e
False
```

Oh, wait! Let go back to `scoped (uses (named "buenosAires")) "aBird"  e`.  We know, it is true that it does not use **exactly** that variable, `aPlace` does use `buenosAires`! Wouldn't it be sweet to be transitive?

```haskell
> transitive (uses (named "buenosAires")) "aBird"  e
True
```

Here we can see another _inspections combinator_: `transitive`, which inspects the given context and all the contexts that are refered from it.

_Contexts_ can be nested, too: for example, if you want to know whether `aBird.position` _uses_ `aPlace` - ignoring that `weight` attribute:

```haskell
> scopedList (uses (named "aPlace")) ["aBird", "position"]  e
True
> scopedList (uses (named "aPlace")) ["aBird", "weight"]  e
False
```

Nice, we know. But not very awesome, it only can tell you if you are using a _identifier_, right? Eeer. Good news, it can tell you much much much more things. See the [supported inspections list](#supported-inspections).

## Identifier predicates

In previous examples, we have always combined the `uses` inspection with the `named` function, but what does `named` mean?

Many inspections support an _identifier predicate_, that is, a matcher for identifier. It can be one of the following:

* `anyone`: true for all identifiers
* `except`: true for any identifier different to the given one
* `like`: true for any identifier that contains the given one
* `named`: true for only the given identifier
* `andAlso`: identifier predicates combiner. True when both predicates are True
* `anyOf`: identifier predicates combiner. True when any of the predicates are True

For example, does the former piece of code declare any attribute?

```haskell
> declaresAttribute anyone e
True
```

Does it declare an attribute like `eight`?

```haskell
> declaresAttribute (like "eight") e
True
```

And does `aBird` use any if within its definition?

```haskell
> scoped usesIf "aBird" e
False
```

## Detections

Let's suppose we want to knoe whether something returns null in the following code:

```haskell
let e = js "var bar = {baz: function(){ return g }, foo: function(){ return null }}"
```

We could manually check if it is the `foo` method in `bar` or the `baz` method in `bar`:

```haskell
> scopedList returnsNil ["bar", "foo"] e
True
> scopedList returnsNil ["bar", "baz"] e
False
```

But instead of asking one by one, we could use `detect` :

```haskell
> detect returnsNil e
["bar","foo"]
-- This means that there are null returns within  `bar` and also within `foo`.
```

`detect` converts an _inspection_ into a _detection_: a function that tells which identifier match a given criteria.


# Supported inspections

The power of Mulang is grounded on more than 70 different kind of inspections:

| Inspection                        | Paradigm           | Meaning
|-----------------------------------|--------------------|------------------------------------------------------
| `assigns`                         |  any               | the given variable or attribute assigned?
| `calls`                           |  any               | is the given method, function or procedure called?
| `declares`                        |  any               | is the given element declared?
| `declaresAttribute`               |  object oriented   | is a given attribute declared?
| `declaresClass`                   |  object oriented   | is a given class declared?
| `declaresComputation`             |  any               | that is, does the given computation  - method, predicate, function, etc - exist?
| `declaresComputationWithArity`    |  any               |  that is, does the given computation have the exact given arity?
| `declaresEntryPoint`              |  any               | is there a program entry point, like a `main` procedure?
| `declaresEnumeration`             |  imperative        | is a given enumeration declared?
| `declaresFact`                    |  logic             | is a given logic fact declared?
| `declaresFunction`                |  functional/imperative | is a given function declared?
| `declaresInterface`               |  object oriented   | is a given interface declared?
| `declaresMethod`                  |  object oriented   | is a given method declared?
| `declaresObject`                  |  object oriented   | is a given named object declared?
| `declaresPredicate`               |  logic             | is a given rule o fact declared?
| `declaresProcedure`               |  imperative        | is a given procedure declared?
| `declaresRecursively`             |  any               | is a given computation declared using recusion?
| `declaresRule`                    |  logic             | is a given logic rule declared?
| `declaresSuperclass`              |  object oriented   | is a given class declared as superclass?
| `declaresTypeAlias`               |  any               | is a given type synonym declared?
| `declaresTypeSignature`           |  any               | is a given computation type signature declared?
| `declaresVariable`                |  any               | is a given local o global variable declared?
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
| `returnsNill`                     |
| `typesAs`                         |  any             | is the given type used to type a variable?
| `typesParameterAs`                |  any               | is a parameter typed as a given type?
| `typesReturnAs`                   |  any               | is the given type used to type a return?
| `uses`                            |  any               | is there any reference to the given element?
| `usesAnonymousVariable`           |
| `usesComposition`                 |
| `usesConditional`                 |
| `usesCut`                         |  logic             | is the logic `!` consult used?
| `usesDyamicPolymorphism`          |  object oriented | are there two or more methods definitions for some sent selector?
| `usesDynamicMethodOverload`       |  object oriented | is there a class that defined two methods with different arity but with the same name?
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
| `usesLoop`                        |  procedural        | are any of: repeat / for loop / foreach / while used?
| `usesMixins`                      |  object oriented   | is any mixins explicitly included?
| `usesNot`                         |
| `usesObjectComposition`           |  object oriented | is there a class that declares an attributes and sends a message to it?
| `usesPatternMatching`             |
| `usesRepeat`                      |
| `usesStaticMethodOverload`        |  object oriented | is there a class that defined two method signatures but with the same name?
| `usesStaticPolymorphism`          |  object oriented | is there an interface with at least a method signature that is implemented by two or more classes and used in the code?
| `usesSwitch`                      |
| `usesTemplateMethod`              |  object oriented | is there a class that sends a message whose corresonding method is not declared?
| `usesType`                        |  any             | is the given typed used in a signature?
| `usesUnificationOperator`         |  logic             | is the logic unification operator `=` used?
| `usesWhile`                       |  imperative        | is a `while` control structure used?
| `usesYield`                       |  functional        | is an expression yielded within a comprehension?


# Supported languages

Mulang is an universal tool which can work with many different programming languages. it natively supports:

  * Haskell
  * Java
  * JavaScript (ES5)
  * Python
  * Prolog

In addition, through external tools, it offers support for the following languages:

  * Ruby, using [mulang-ruby](https://github.com/mumuki/mulang-ruby)
  * PHP, using [mulang-php](https://github.com/mumuki/mulang-php)
  * Gobstones, using [gs-weblang-cli](https://github.com/gobstones/gs-weblang-cli)

So in order to use it with a particular language, you have to:

* either add explicit support in this repo, or
* translate your language into one of the natively supported ones, or
* translate your language to the Mulang JSON AST

# Command Line Tool

You can also use Mulang from the Command Line, without having to interact with Haskell code. This tool allows to perform most common analysis out of the box by using a JSON spec. It supports four different kinds of analysis:

1. **Expectation analysis**: you can pass _inspections_ that will be tested against the provied program. Expectations answer questions like: _does the function X call the function Y?_ or _does the program use if's?_.
4. **Smell analysis**: instead of asking explcit questions to the program, the smells analysis implicitly runs specific inspections - that denote bad code - in orden to know if any of them is matched.
2. **Intermediate Language analysis**: you can ask the tool to generate the Mulang AST for a given source code.
3. **Signature analysis**: report the signatures of the computations present in source code.

## The expectations DSL

In order to pass expectations to the Command Line Tool, you must use a simple DSL that builds the inspections for you.

| Kind        | DSL Sample                  | Haskell Combinators Sample
|-------------|-----------------------------|----------------------------
| Basic       | `* UsesIf`                  |  `usesIf`
| Negated     | `* Not:UsesWhile`           | `(negative usesWhile)`
| Predicated  | `* DeclaresClass:Foo`       | `(declaresClass (named "Foo"))`
|             | `* DeclaresClass:=Foo`      | `(declaresClass (named "Foo"))`
|             | `* DeclaresClass:~Foo`      | `(declaresClass (like "Foo"))`
|             | `* DeclaresClass:^Foo`      | `(declaresClass (except "Foo"))`
|             | `* DeclaresClass:[Foo\|Bar]` | `(declaresClass (anyOf ["Foo", "Bar"]))`
|             | `* DeclaresClass:*`         | `(declaresClass anyone)`
|             | `* DeclaresClass`           | `(declaresClass anyone)`
| Transitive  | `foo UsesLambda`            | `(transitive usesLambda "foo")`
| Scoped      | `Intransitive:foo UsesIf`   | `(scoped usesIf "foo")`
| Scoped List | `foo.bar UsesIf`            | `(scopedList usesIf ["foo", "bar"])`


## Examples

Let's see some usage samples:

### With intransitive expectations

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : ":Intransitive:x",
            "inspection" : "Uses:*"
         }
      ]
   }
}
' | json_pp
{
   "expectationResults" : [
      {
         "expectation" : {
            "binding" : ":Intransitive:x",
            "inspection" : "Uses:*"
         },
         "result" : false
      }
   ],
   "smells" : [],
   "tag" : "AnalysisCompleted",
   "signatures" : []
}
```

### With unscoped expectations

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : "*",
            "inspection" : "Declares:x"
         }
      ]
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "smells" : [],
   "expectationResults" : [
      {
         "result" : true,
         "expectation" : {
            "binding" : "*",
            "inspection" : "Declares:x"
         }
      }
   ],
   "signatures" : []
}
```

### With signature analysis

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return x + y; }"
   },
   "spec" : {
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
}
' | json_pp
{
   "expectationResults" : [],
   "smells" : [],
   "signatures" : [
      "-- foo x y"
   ],
   "tag" : "AnalysisCompleted"
}
```

### With broken input

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "JavaScript",
      "content" : "function foo(x, y { return x + y; }"
   },
   "spec" : {
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
}' | json_pp
{
   "tag" : "AnalysisFailed",
   "reason" : "Sample code parsing error"
}
```

### With AST as input

```bash
$ mulang '
{
   "sample" : {
      "tag" : "MulangFragment",
      "ast" : {
         "tag" : "Sequence",
         "contents" : [
            {
              "tag" : "Variable",
              "contents" : [
                "x",
                { "tag" : "MuNumber", "contents" : 1 }
              ]
            },
            {
              "tag" : "Variable",
              "contents" : [
                "y",
                { "tag" : "MuNumber", "contents" : 2 }
              ]
            }
         ]
      }
   },
   "spec" : {
      "signatureAnalysisType" : {
         "tag" : "StyledSignatures",
         "style" : "HaskellStyle"
      }
   }
}
' | json_pp
{
   "expectationResults" : [],
   "smells" : [],
   "tag" : "AnalysisCompleted",
   "signatures" : [
      "-- x",
      "-- y"
   ]
}
```

### With Smell Analysis, by inclusion

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "smellsSet" : {
        "tag" : "NoSmells",
        "include" : [
          "ReturnsNil",
          "DoesNullTest"
        ]
      },
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [],
   "signatures" : [
      "-- foo x y"
   ],
   "smells" : [
      {
         "binding" : "foo",
         "inspection" : "ReturnsNil"
      }
   ]
}
```

### With Smell Analysis, by exclusion

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "smellsSet" : {
        "tag" : "AllSmells",
        "exclude" : [
          "ReturnsNil"
        ]
      },
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
}
' | json_pp
{
   "smells" : [],
   "signatures" : [
      "-- foo x y"
   ],
   "tag" : "AnalysisCompleted",
   "expectationResults" : []
}
```

### With expressiveness smells

Expressivnes smells are like other smells - they can be included or excluded using the `smellsSet` settings. However, their behaviour is also controlled
by the `domainLanguage` setting, which you _can_ configure:

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "Prolog",
      "content" : "son(Parent, Son):-parentOf(Son, Parent).parentOf(bart, homer)."
   },
   "spec" : {
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : {
         "caseStyle" : "SnakeCase",
         "minimumIdentifierSize" : 4,
         "jargon" : ["id"]
      }
   }
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "signatures" : [],
   "smells" : [
      {
         "inspection" : "HasTooShortIdentifiers",
         "binding" : "son"
      },
      {
         "binding" : "parentOf",
         "inspection" : "HasWrongCaseIdentifiers"
      }
   ],
   "expectationResults" : []
}
```

Also, if you want to use `HasMisspelledIdentifiers` smell, you _need_ to specify a dictionary - with must be ordered, downcased and with unique words only:

```bash
$ mulang  '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : { "dictionaryFilePath" : "/usr/share/dict/words" }
   }
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [],
   "signatures" : [],
   "smells" : [
      {
         "inspection" : "ReturnsNil",
         "binding" : "foo"
      },
      {
         "inspection" : "HasMisspelledIdentifiers",
         "binding" : "foo"
      }
   ]
}
```


### With Intermediate Language Generation

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeFragment",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "includeIntermediateLanguage" : true
   }
}
' | json_pp
{
   "expectationResults" : [],
   "smells" : [],
   "tag" : "AnalysisCompleted",
   "signatures" : [],
   "intermediateLanguage" : {
      "tag" : "Function",
      "contents" : [
         "foo",
         [
            [
               [
                  {
                     "tag" : "VariablePattern",
                     "contents" : "x"
                  },
                  {
                     "tag" : "VariablePattern",
                     "contents" : "y"
                  }
               ],
               {
                  "tag" : "UnguardedBody",
                  "contents" : {
                     "contents" : {
                        "tag" : "MuNil"
                     },
                     "tag" : "Return"
                  }
               }
            ]
         ]
      ]
   }
}

```

## With test running

```bash
mulang '{
"sample" : {
   "tag" : "CodeFragment",
   "language" : "JavaScript",
   "content" : "function f(x) { return x + 1 }"
},
"spec" : {
   "testAnalysisType" : {
     "tag" :  "ExternalTests",
     "test" : {
       "tag" : "CodeFragment",
       "language" : "JavaScript",
       "content" : "it(\"f increments by one\", function() { assert.equals(f(1), 2) })"
     }
   }
 }
}' | json_pp
{
   "testResults" : [
      {
         "description" : [
            "f increments by one"
         ],
         "status" : {
            "tag" : "Success"
         }
      }
   ],
   "signatures" : [],
   "intermediateLanguage" : null,
   "tag" : "AnalysisCompleted",
   "smells" : [],
   "expectationResults" : []
}
```
For further detail on this spec, see [Code Execution](#code-execution)

# Mulang AST spec

In this section, we will get into the technical details of the Mulang AST. It is built around 5 core elements:

* [Expressions](#expressions)
* [Patterns](#patterns)
* [Types](#types)
* Equations
* Generators

All the AST elements fall within any of these 5 categories.

## Expressions

Expressions are the most important element kind, since contain most of the information of a Mulang program and are always the root element of it. In fact, this implementation does not contain an `AST` or `Program` datatype - it is instead types as `Expression`.

Expression in Mulang model what you will normally spec in a language as a expression, that is something that holds a value and a type. For example, `4 + 5` and `[2, 3].toString()` are typical expresion.

However, Mulang extends this concept to most kind of elements in a program, regadless they are have an actual value in the original language. For example, class declarations and while statements are modeled as expression, although in many languages they aren't.

As a rule of thumb if something is or can be represented as an statement, declararion or expression, the it is modeled as `Expression` in Mulang AST.

### `Record`

> A `Record` represents a record, data or struct declaration,
> as found in most procedural and functional languages, like the C-like `struct` declaration

#### Syntax

```haskell
(Record Identifier)
```

#### C Example

```c
struct Point {
  int x;
  int y;
}
```

```haskell
(Record "Point")
```

#### Caveats

Currently, the `Record` expression does not hold information about the record contents.


### `TypeAlias`, `TypeSignature` and `TypeCast`

Mulang AST support for type analysis is quite limited, and it is mostly focused on expressions and declarations analysis. However, for sake of completeness and in order to provide some limited type-information in Mulang AST, `TypeAlias`, `TypeSignature` and `TypeCast` expressions are provided.

See [types section](#types) for more details.

### `EntryPoint`

> Entry point with its name and body. It typically correspond to C-like `main` procedures, or `program` declarations.

#### Syntax

```haskell
(EntryPoint Identifier Expression)
```

#### Java Example

```java
public static main(String[] args) {}
```

```haskell
(EntryPoint "main" MuNil)
```

### `Function`

> Functional / Imperative programming function declaration.
> It is is composed by an identifier and one or more equations

#### Syntax

```haskell
(Function Identifier [Equation])
```

#### Example

### `Procedure`

> Imperative programming procedure declaration. It is composed by an identifier and one or more equations

#### Syntax

```haskell
(Procedure Identifier [Equation])
```

### `Method`

> Object oriented programming method declaration. It is composed by an identifier and one or more equations


#### Syntax

```haskell
(Method Identifier [Equation])
```

#### Ruby Example

```ruby
class Bird
  def sing!
    puts "singing in the dead of night"
  end
end
```

```haskell
(Class
  "Bird" Nothing
  (Method
      "sing!"
      (Equation []
          (UnguardedBody (Print (MuString "singing in the dead of night"))))))
```

#### Java Example

```java
public class Bird {
  public void sing() {
    System.out.println("singing in the dead of night");
  }
}
```

```haskell
(Class
  "Bird" Nothing
  (Method
      "sing"
      (Equation []
          (UnguardedBody (Print (MuString "singing in the dead of night"))))))
```


### `EqualMethod` and `HashMethod`

> Declaration of custom equivalance and _hash code_ operations. `EqualMethod` typically corresponds to `equals` or `==` method declarations, while `HashMethod`, typically corresponds
to `hash` or `hashCode`- like methods.

#### Syntax

```haskell
(EqualMethod [Equation])
```

```haskell
(HashMethod [Equation])
```

#### Ruby Example

```ruby
def ==(other)
end

def hash
end
```

```haskell
(Sequence [
  (EqualMethod (Equation
                [VariablePatten "other"]
                (UnguardedBody MuNil))),
  (HashMethod (Equation
              []
              (UnguardedBody MuNil)))]

```

### `Variable`

> Generic variable declaration, composed by an identifier and an initializer

#### Syntax

```haskell
(Variable Identifier Expression)
```

#### Example

### `Assignment`

#### Syntax

```haskell
(Assignment Identifier Expression)
```

#### Example

### `Attribute`

> Object oriented programming attribute declaration, composed by an identifier and an initializer

#### Syntax

```haskell
(Attribute Identifier Expression)
```

#### Example

### `Object`

> Object oriented programming global, named object declaration, like Scala's `object`, composed by a name and a body.

#### Syntax

```haskell
(Object Identifier Expression)
```

#### Example

### `Class`

> Object oriented programming global, class declaration,
> composed by a name, an optional superclass and a body

#### Syntax

```haskell
(Class Identifier (Maybe Identifier) Expression)
```

#### Ruby Example

```ruby
class Bird < Animal
end
```

```haskell
(Class "Bird" (Just "Animal") MuNil)
```

#### Java Examples

```java
public class Bird extends Animal {}
```

```haskell
(Class "Bird" (Just "Animal") MuNil)
```

### `Enumeration`

> Imperative named enumeration of values

#### Syntax

```haskell
(Enumeration Identifier [Identifier])
```


#### Java Example

```java
public enum Fuzzy {
  YES, NO, MAYBE
}
```

```haskell
(Enumeration "Fuzzy" ["YES", "NO", "MAYBE"])
```

### `Interface`

> Object oriented programming global interface or contract declaration, composed by a name, superinterfaces and a body.

#### Syntax

```haskell
(Interface Identifier [Identifier] Expression)
```

#### Java Example

```java
public interface Foo extends Bar, Baz {
  void foo();
}
```

```haskell
(Interface
    "Foo"
    ["Bar", "Baz"]
    (TypeSignature "foo" [] "void"))
```

### `Rule`

> Logic programming declaration of rule fact, composed by the rule name, rule arguments, and rule body

#### Syntax

```haskell
(Rule Identifier [Pattern] [Expression])
```

#### Example

```prolog
baz(bar) :- foo(bar)
```

```haskell
(Rule "baz"
    [(LiteralPattern "bar")]
    [(Exist "foo"
        [(LiteralPattern "bar")])])
```

### `Fact`

> Logic programming declaration of a fact , composed by the fact name and fact arguments

#### Syntax

```haskell
(Fact Identifier [Pattern])
```

#### Example

```prolog
foo(bar).
```

```haskell
(Fact "foo" [(LiteralPattern "bar")])
```

### `Exist`

> Logic programming existential cuantification / consult

#### Syntax

```haskell
(Exist Identifier [Pattern])
```


#### Example

### `Not`

> Logic programming negation

#### Syntax

```haskell
(Not Expression)
```


#### Example

### `Findall`

> Logic programming findall

#### Syntax

```haskell
(Findall Expression Expression Expression)
```


#### Example

### `Forall`

> Logic programming universal cuantification

#### Syntax

```haskell
(Forall Expression Expression)
```


#### Example

### `Reference`

> Generic variable

#### Syntax

```haskell
(Reference Identifier)
```


#### Example

### `Application`

> Generic, non-curried application of a function or procedure,
> composed by the applied element itself, and the application arguments

#### Syntax

```haskell
(Application Expression [Expression])
```

#### Example

### `Send`

> Object oriented programming message send, composed by the reciever,
> selector and arguments

#### Syntax

```haskell
(Send Expression Expression [Expression])
```

#### Ruby Example

```ruby
1 + 5
```

```haskell
(Send (MuNumber 1) (Reference "+") [MuNumber 5])
```

### `New`

> Object oriented instantiation, composed by the class reference and instantiation arguments

#### Syntax

```haskell
(New Identifier [Expression])
```

#### Example

### `Implement`

> Object oriented instantiation, interface implementation

#### Syntax

```haskell
(Implement Identifier)
```


#### Example

### `Include`

> Object oriented instantiation, mixin inclusion

#### Syntax

```haskell
(Include Identifier)
```


#### Example

### `If`

#### Syntax

```haskell
(If Expression Expression Expression)
```
### `Lambda`

#### Syntax

```haskell
(Lambda [Pattern] Expression)
```

### `Return`

#### Syntax

```haskell
(Return Expression)
```

### `While`

> Imperative programming conditional repetition control structure, composed by a condition and a body

#### Syntax

```haskell
(While Expression Expression)
```

#### Example

### `Repeat`

> Imperative programming fixed repetition control structure, composed by a repetition count expression, and a body

#### Syntax

```haskell
(Repeat Expression Expression)
```

#### Example

### `Match`

#### Syntax

```haskell
(Match Expression [Equation])
```

### `Switch`

#### Syntax

```haskell
(Switch Expression [(Expression, Expression)])
```

### `Try`

> Generic try expression, composed by a body, a list of exception-handling patterns and statments, and a finally expression

#### Syntax

```haskell
(Try Expression [(Pattern, Expression)] Expression)
```


#### Example

### `Raise`

> Generic raise expression, like a throw or raise statament, composed by the raised expression

#### Syntax

```haskell
(Raise Expression)
```

#### Example

### `Print`

> Generic print expression

#### Syntax

```haskell
(Print Expression)
```

#### Ruby Example

```ruby
puts "Hello World"
```

```haskell
(Print (MuString "Hello World"))
```

### `For`

> `For`s generalices the concept of comprehensions an indexed repetition. With a `For` you can build:
>
> * `ForComprehension`, when the for expression is a yield. Scala's `for` comprehensions, Erlang's and Haskell's list comprehensions, and Haskell's `do-syntaxt` map to it.
> * `ForEach`, when the for expression is not a yield.  Java's `for:`, or some scenarios of scala's `for` map to it.

#### Syntax

```haskell
(For [Statment] Expression)
```

#### Haskell Example

```haskell
m = [ f x | x <- [1, 2, 3, 4] ]
```

```haskell
(Variable "m"
  (For
    [(Generator
      (VariablePattern "x")
      (MuList [(MuNumber 1), (MuNumber 2), (MuNumber 3), (MuNumber 4)]))]
    (Yield
      (Application (Reference "f") [(Reference "x")]))))
```

#### Java Example

```java
for (Integer i : ints) {
  System.out.println(i);
}
```

```haskell
(For
  [(Generator
      (VariablePattern "i")
      (Reference "ints"))]
  (Print (Reference "i")))
```

### `ForLoop`

> `ForLoop` represents the imperative programming c-style for loop:

#### Syntax

```haskell
(ForLoop Expression Expression Expression Expression)
```

#### Example

```javascript
for (var i = 0; i < 10; i++) {
  console.log(i);
}
```

```haskell
(ForLoop
  (Variable "i" (MuNumber 0.0))
  (Application (Reference "<") [Reference "i",MuNumber 10.0])
  (Assignment "i" (Application (Reference "+") [Reference "i",MuNumber 1.0]))
  (Send (Reference "console") (Reference "log") [Reference "i"]))
```

### `Sequence`

> Generic sequence of statements

#### Syntax

```haskell
(Sequence [Expression])
```

#### Example

### `Other`

#### Syntax

```haskell
(Other)
```

### `Equal` and `NotEqual`

#### Syntax

```haskell
(Equal)
(NotEqual)
```

### `Self`

> Object oriented self-reference, like  C-like `this` and Smalltalk-derived `self`

#### Syntax

```haskell
(Self)
```

### `None`

> Used as a placeholder for empty bodies.

#### Syntax

```haskell
(None)
```

### `MuNil`

> Generic nothing value literal - `nil`, `null`, `()` or `unit`.

#### Syntax

```haskell
(MuNil)
```

### `MuObject`

> Object oriented unnamed object literal

#### Syntax

```haskell
(MuObject Expression)
```

#### JavaScript Example

```javascript
{}
{foo: 1}
{foo: 1, bar: 2}
```

```haskell
(MuObject MuNil)
(MuObject (Attribute "foo" (MuNumber 1)))
(MuObject (Sequence [
            (Attribute "foo" (MuNumber 1)),
            (Attribute "bar" (MuNumber 2))]))
```

### `MuNumber`, `MuBool`, `MuString`, `MuSymbol` and `MuChar`

> Generic number, boolean, string, symbol (atoms) and char literals

#### Syntax

```haskell
(MuNumber Double)
```

```haskell
(MuBool Bool)
```

```haskell
(MuString String)
```

```haskell
(MuSymbol String)
```

```haskell
(MuChar Char)
```

#### Ruby Example

```ruby
1
true
"hello"
:hello
```

```haskell
(Sequence [
  (MuNumber 1),
  (MuBool True),
  (MuString "hello"),
  (MuSymbol "hello")])
```

### `MuTuple` and `MuList`

> They represent tuples - generic non-uniform fixed-size collection of elements - and lists - generic uniform variable-size collection of elements.
> Lists typically map to arrays, lists or sequence-like structures.

#### Syntax

```haskell
(MuTuple [Expression])
```

```haskell
(MuList [Expression])
```

### `TestGroup`, `Test` and `Assert`

> Generic test framework expressions used to represent unit tests.
> TestGroup represents a test grouping expression such as `describe`, `context`, etc
> Test represents a test expression such as `it`, etc
> Assert represents a test's assertion, such as `assert.equals(...)`, etc. It receives a boolean that represents whether the assertion is negated or not.

#### Syntax

```haskell
(TestGroup Expression Expression)
```

```haskell
(Test Expression Expression)
```

```haskell
(Assert Bool Assertion)
```

#### Javascript Example

```javascript
describe("succ", function() {
  it("succ of 3 is 4", function() {
    assert.equals(succ(3), 4)
  })
})
```

```haskell
TestGroup (MuString "succ")
  (Test (MuString "succ of 3 is 4")
    (Assert False (Equality (Application (Reference "succ") [MuNumber 3.0]) (MuNumber 4.0))))
```

#### Python Example

```python
class TestGroup(unittest.TestCase):
  def test_succ_of_3_is_4():
    self.assertEqual(succ(3), 4)
```

```haskell
TestGroup (MuString "TestGroup")
  (Test (MuString "test_succ_of_3_is_4")
    (Assert False (Equality (Application (Reference "succ") [MuNumber 3.0]) (MuNumber 4.0))))
```

## Assertion

Assertions used within tests to dynamically ascertain the code's validity.

An assertion can be one of:
 * `Truth`: Assert the truthfulness of a given expression.
 * `Equality`: Assert the equality of two given expressions.
 * `Failure`: Assert a given expression fails with a given error.

#### Syntax

```haskell
(Truth Expression)
```

```haskell
(Equality Expression Expression)
```

```haskell
(Failure Expression Expression)
```

#### Javascript Examples

```javascript
assert(true)
```

```haskell
Assert False (Truth (MuBool True))
```

```javascript
assert.equals(3, 3)
```

```haskell
Assert False (Equality (MuNumber 3) (MuNumber 3))
```

```javascript
assert.throws(function() { throw('error!') }, 'error!')
```

```haskell
Assert False (Failure (Lambda [] (Raise (MuString "error!"))) (MuString "error!"))
```

## Patterns

Patterns are the second most important element of Mulang AST. They represent things that don't hold a value, but are instead used to match values, like
patterns in imperative `case` or `switch` statements, functional pattern matching in `match` or `case` expressions, or exception matching in `try-catch` or `begin-rescue`-like statements in object oriented languages.

### `VariablePattern`

> Variable pattern represent a variable match. It corresponds to normal formal parameters in precedural languages,
> and to simple pattern matching against a free identifier.

#### Syntax

```haskell
(VariablePattern String)
```

#### JavaScript Example

```javascript
function foo(x, y) { }
```

```haskell
(Function "foo"
  [(Equation
      [(VariablePattern "x"), (VariablePattern "y")]
      (UnguardedBody MuNil))])
```

### `LiteralPattern`

> Literal constant pattern

#### Syntax


```haskell
(LiteralPattern String)
```

#### Example

### `InfixApplicationPattern`

> Infix application pattern like `4:X`

#### Syntax


```haskell
(InfixApplicationPattern Pattern String Pattern)
```
##### Caveats

`InfixApplicationPattern` exposes the underying syntax and will be deprecated.

### `ApplicationPattern`

> prefix application pattern like `f _`

#### Syntax


```haskell
(ApplicationPattern String [Pattern])
```

#### Example

### `TuplePattern`

> tuple pattern like `(3, _)`

#### Syntax


```haskell
(TuplePattern [Pattern])
```

#### Example

### `ListPattern`

> list pattern like `[x, y, _]`

#### Syntax


```haskell
(ListPattern [Pattern])
```

#### Example

### `FunctorPattern`

> Prolog-like functor pattern, like `f(X, 6)`.

#### Syntax

```haskell
(FunctorPattern Identifier [Pattern])
```

#### Example

### `AsPattern`

#### Syntax

```haskell
(AsPattern Identifier Pattern)
```

#### Example

### `TypePattern`

> A type pattern, like in exception handling constructs in most object-oriented languages

#### Syntax

```haskell
(TypePattern Identifier)
```

#### Example

### `WildcardPattern`

> Wildcard pattern, typically `_` in functional an logic programming languages.

#### Syntax

```haskell
(WildcardPattern)
```

### `UnionPattern`

#### Syntax

```haskell
(UnionPattern [Pattern])
```

### `OtherPattern`

> Other unrecognized pattern

#### Syntax

```haskell
(OtherPattern)
```

## Types

When processing statically-typed languages, all type-information - regardless we are typing a function, a variable or a class - is represented with the `Type` ADT, can be one of:

  * `SimpleType`: composed by a type identifier and zero or type more constraints
  * `ParameterizedType`: composed by input type parmaters, an output type, and type constratins
  * `ConstrainedType`: composed by just type constraints.
  * `OtherType`: an unrecognized type

`Type`s can be introduced in the Mulang AST using the following elements:

### `TypeAlias`

> A `TypeAlias` represents a synonym for a type, like the `type` declaration in Haskell and Scala or C's `typedef`.
> It is a typical statically typed functional programming feature.

#### Syntax

```haskell
(TypeAlias Identifier Identifier)
```

#### Haskell Example

```haskell
type Point = (Int, Int)
```

```haskell
(TypeAlias "Point" "(Int, Int)")
```

### TypeSignature

> A `TypeSignature` represents an explicit type annotation for a computation,
> variable or module, as you can find in Java or Haskell.

#### Syntax

```haskell
(TypeSignature Identifier Type)
```

#### Haskell Examples

Simple types:

```haskell
name :: String
```

```haskell
(TypeSignature "name" (SimpleType "String" []))
```

Simple types and constraints:

```haskell
f :: Num a => a
````

```haskell
(TypeSignature "f" (SimpleType "a" ["Num a"]))
```

Parameterized types:


```haskell
elem :: (Eq a, Foldable t) => a -> t a -> Bool
````

```haskell
(TypeSignature "elem" (ParameterizedType ["a", "t a"] "Bool" ["Eq a", "Foldable t"]))
```

#### Java Examples

In Java, as in most typed C-like languages, type signature and variable declarations are bound. This means that, for example, a local variable declaration will produce both a `TypeSignature` and a `Variable` expression.

Variable and attribute types:

```java
String name;
```

```haskell
(TypeSignature "name" (SimpleType "String" []))
```

Method types:

```java
String f() { return null; }
```

```haskell
(TypeSignature "f" (ParameterizedType [] "String" []))
```

Method types with type parameters:

```java
<A> A f() { return null; }
```

```haskell
(TypeSignature "f" (ParameterizedType [] "A" ["A"]))
```

Method types with type parameters and constraints:

```java
<A super B> void f(A a) {}
```

```haskell
(TypeSignature "f" (ParameterizedType ["A"] "void" ["A super B"]))
```

Class or interfaces types:

```java
class A<B extends C, D extends C> { }
```

```haskell
(TypeSignature "A" (ConstrainedType ["B extends C", "D extends C"]))
```


### `TypeCast`

> A `TypeCast` represent explictly giving a type to an expression
> which may have static or dynamic impact on the program. It is aimed to represent
> type-casts in c-like languages and inline type signatures in functional languages.

#### Syntax

```haskell
(TypeCast Expression Type)
```

#### Haskell Examples

Simple types:

```haskell
... = 4 :: Num a => a
```

```haskell
(TypeCast (MuNumber 4) (SimpleType "a" ["Num a"]))
```

#### Java Examples

Variable and attribute types:

```java
(Integer) 4;
```

```haskell
(TypeCast (MuNumber 4) (SimpleType "Integer" []))
```

```java
(Option<Integer>) something;
```

```haskell
(TypeCast (Reference "something") (SimpleType "Option<Integer>" []))
```

##### Caveats

The type constraints refer to type-constrained parametrizations that the cast introduces, and
not any other kind of constraints the cast uses. That is whay the following Java code:

```java
(Num<A>) something;
```

produces:

```haskell
(TypeCast (Reference "something") (SimpleType "Num<A>" []))
```

instead of:

```haskell
(TypeCast (Reference "something") (SimpleType "Num" ["A"]))
```

# Code Execution

As of v4.4.0, mulang provides basic support for executing its AST.
This feature can accessed through a `testAnalysisType` spec, such as the one shown in [this section](#with-test-running).

Currently, support is given for executing the following AST elements:

- [Application](#application)
- [Assert](#testgroup-test-and-assert)
- [Assignment](#assignment)
- [ForLoop](#forloop)
- [If](#if)
- [Lambda](#lambda)
- [MuBool](#munumber-mubool-mustring-musymbol-and-muchar)
- [MuList](#mutuple-and-mulist)
- [MuNil](#munil)
- [MuNumber](#munumber-mubool-mustring-musymbol-and-muchar)
- [MuString](#munumber-mubool-mustring-musymbol-and-muchar)
- [Print](#print)
- [Raise](#raise)
- [Reference](#reference)
- [Return](#return)
- [Sequence](#sequence)
- [Function](#function)
- [Procedure](#procedure)
- [Method](#method)
- [Variable](#variable)
- [While](#while)

### Examples

```bash
mulang '{
  "sample" : {
    "tag" : "CodeFragment",
    "language" : "JavaScript",
    "content" : "
      function f(x) {
        return x + 1
      }"
  },
  "spec" : {
    "testAnalysisType" : {
      "tag" :  "ExternalTests",
      "test" : {
        "tag" : "CodeFragment",
        "language" : "JavaScript",
        "content" : "
          it(\"f increments by one\", function() { 
            assert.equals(f(1), 2)
          })"
      }
    }
  }
}' | json_pp
{
   "testResults" : [
      {
         "status" : {
            "tag" : "Success"
         },
         "description" : [
            "f increments by one"
         ]
      }
   ],
   "signatures" : [],
   "smells" : [],
   "intermediateLanguage" : null,
   "expectationResults" : [],
   "tag" : "AnalysisCompleted"
}
```

Since both the code and tests are parsed to and run as an AST, the two of them needn't be in the same language:

```bash
mulang '{
  "sample" : {
    "tag" : "CodeFragment",
    "language" : "Python",
    "content" : "def f():
        x = 0
        while x < 10:
          x += 1
        return x"
  },
  "spec" : {
    "testAnalysisType" : {
      "tag" :  "ExternalTests",
      "test" : {
        "tag" : "CodeFragment",
        "language" : "JavaScript",
        "content" : "
          it(\"f returns 10\", function() { 
            assert.equals(f(), 10)
          })"
      }
    }
  }
}' | json_pp
{
   "signatures" : [],
   "expectationResults" : [],
   "testResults" : [
      {
         "status" : {
            "tag" : "Success"
         },
         "description" : [
            "f returns 10"
         ]
      }
   ],
   "smells" : [],
   "tag" : "AnalysisCompleted",
   "intermediateLanguage" : null
}

```

# Building mulang from source

## Setup

To generate `mulang` executable, you have to build the project using [stack](https://haskellstack.org):

1. Install stack: `wget -qO- https://get.haskellstack.org/ | sh`
2. Go to the mulang project directory and setup it: `stack setup`
3. Build the project: `stack build`

## Installing and creating an executable


```bash
$ stack install
$ mulang
```

That will generate a `mulang` executable in the folder `~/.local/bin`.

## Running tests

```bash
$ stack test --fast
```

## Watching changes


```bash
$ stack test --fast --file-watch
```

## Loading mulang in the REPL

```bash
stack ghci
```

# Ruby wrapper

This module can also be deployed a ruby gem. `mulang` works with Ruby 2.3.1

```bash
cd gem
rake wrapper:wrap
bundle install
bundle exec rspec
```

See `gem/README` for more details.

# JavaScript library

`mulang` can also be compiled to JavaScript library using [ghcjs](https://github.com/ghcjs/ghcjs) and [ghcjslib](https://github.com/flbulgarelli/ghcjslib), which allows you to use it from `node` or the browser.

> :warning: you will need `node >= 7` installed on your system. If you have `nvm`, before starting run the following:
>
> ```sh
> $ nvm use $(cat ghcjslib/.nvmrc)
>```

1. Run `ghcjslib/swap.sh` for swapping to GHCJS compiler
2. Run `ghcjslib/build.sh` for building the `ghcjslib` release. It will be placed on `ghcjslib/build/mulang.js`
3. Run `ghcjslib/test.sh` for running both mocha and hspec tests.
4. Load it:
   1. in the browser: `google-chrome ghcjslib/index.html`
   2. in `node`: run `node`, and then, within the interpreter, run: `let mulang = require('./ghcjslib/build/mulang.js');`
5. Try it: `mulang.analyse(...pass here a spec as described in the README....)`
6. Run `ghcjslib/swap.sh` again for swapping back to ghc

# Tagging and releasing

```bash
./tag.sh
```

# Contributors

 * Franco Bulgarelli @flbulgarelli [Mumuki](@mumuki)
 * Julian Berbel Alt @julian-berbel @ [Mumuki](@mumuki)
 * Federico Lochbaum @FedeLochbaum @ [UNQ](http://www.unq.edu.ar/)
 * Lucas Traverso @ludat @ [10Pines](@10pines)
