[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

Mulang
======
> A universal, multi-language, multi-paradigm code analyzer

Mulang is three different  - but thighly related - things:

  * an intermediate language, known as the [Mulang AST](#mulang-ast-spec);
  * a [command line tool](#command-line-tool) for analysing the Mulang AST and [some popular languages](#supported-languages) by transforming to it
  * a Haskell composable [combinators library]((#combinators-crash-course)) for analysing the Mulang AST;

# Table of contents

- [Mulang](#mulang)
- [Table of contents](#table-of-contents)
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
- [Mulang AST spec](#mulang-ast-spec)
  * [Expressions](#expressions)
    + [`Record`](#record)
      - [Syntax](#syntax)
      - [C Example](#c-example)
      - [Caveats](#caveats)
    + [`TypeAlias`](#typealias)
      - [Syntax](#syntax-1)
      - [Haskell Example](#haskell-example)
      - [Caveats](#caveats-1)
    + [TypeSignature](#typesignature)
      - [Syntax](#syntax-2)
      - [Haskell Example](#haskell-example-1)
      - [Java Example](#java-example)
      - [Caveats](#caveats-2)
    + [`EntryPoint`](#entrypoint)
      - [Syntax](#syntax-3)
      - [Java Example](#java-example-1)
    + [`Function`](#function)
      - [Syntax](#syntax-4)
      - [Example](#example)
    + [`Procedure`](#procedure)
      - [Syntax](#syntax-5)
    + [`Method`](#method)
      - [Syntax](#syntax-6)
      - [Ruby Example](#ruby-example)
      - [Java Example](#java-example-2)
    + [`EqualMethod` and `HashMethod`](#equalmethod-and-hashmethod)
      - [Syntax](#syntax-7)
      - [Ruby Example](#ruby-example-1)
    + [`Variable`](#variable)
      - [Syntax](#syntax-8)
      - [Example](#example-1)
    + [`Assignment`](#assignment)
      - [Syntax](#syntax-9)
      - [Example](#example-2)
    + [`Attribute`](#attribute)
      - [Syntax](#syntax-10)
      - [Example](#example-3)
    + [`Object`](#object)
      - [Syntax](#syntax-11)
      - [Example](#example-4)
    + [`Class`](#class)
      - [Syntax](#syntax-12)
      - [Ruby Example](#ruby-example-2)
      - [Java Examples](#java-examples)
    + [`Enumeration`](#enumeration)
      - [Syntax](#syntax-13)
      - [Java Example](#java-example-3)
    + [`Interface`](#interface)
      - [Syntax](#syntax-14)
      - [Java Example](#java-example-4)
    + [`Rule`](#rule)
      - [Syntax](#syntax-15)
      - [Example](#example-5)
    + [`Fact`](#fact)
      - [Syntax](#syntax-16)
      - [Example](#example-6)
    + [`Exist`](#exist)
      - [Syntax](#syntax-17)
      - [Example](#example-7)
    + [`Not`](#not)
      - [Syntax](#syntax-18)
      - [Example](#example-8)
    + [`Findall`](#findall)
      - [Syntax](#syntax-19)
      - [Example](#example-9)
    + [`Forall`](#forall)
      - [Syntax](#syntax-20)
      - [Example](#example-10)
    + [`Reference`](#reference)
      - [Syntax](#syntax-21)
      - [Example](#example-11)
    + [`Application`](#application)
      - [Syntax](#syntax-22)
      - [Example](#example-12)
    + [`Send`](#send)
      - [Syntax](#syntax-23)
      - [Ruby Example](#ruby-example-3)
    + [`New`](#new)
      - [Syntax](#syntax-24)
      - [Example](#example-13)
    + [`Implement`](#implement)
      - [Syntax](#syntax-25)
      - [Example](#example-14)
    + [`Include`](#include)
      - [Syntax](#syntax-26)
      - [Example](#example-15)
    + [`If`](#if)
      - [Syntax](#syntax-27)
    + [`Lambda`](#lambda)
      - [Syntax](#syntax-28)
    + [`Return`](#return)
      - [Syntax](#syntax-29)
    + [`While`](#while)
      - [Syntax](#syntax-30)
      - [Example](#example-16)
    + [`Repeat`](#repeat)
      - [Syntax](#syntax-31)
      - [Example](#example-17)
    + [`Match`](#match)
      - [Syntax](#syntax-32)
    + [`Switch`](#switch)
      - [Syntax](#syntax-33)
    + [`Try`](#try)
      - [Syntax](#syntax-34)
      - [Example](#example-18)
    + [`Raise`](#raise)
      - [Syntax](#syntax-35)
      - [Example](#example-19)
    + [`Print`](#print)
      - [Syntax](#syntax-36)
      - [Ruby Example](#ruby-example-4)
    + [`Comprehension`](#comprehension)
      - [Syntax](#syntax-37)
      - [Caveats](#caveats-3)
    + [`Sequence`](#sequence)
      - [Syntax](#syntax-38)
      - [Example](#example-20)
    + [`Other`](#other)
      - [Syntax](#syntax-39)
    + [`Equal` and `NotEqual`](#equal-and-notequal)
      - [Syntax](#syntax-40)
    + [`Self`](#self)
      - [Syntax](#syntax-41)
    + [`MuNull`](#munull)
      - [Syntax](#syntax-42)
      - [Example](#example-21)
    + [`MuObject`](#muobject)
      - [Syntax](#syntax-43)
      - [JavaScript Example](#javascript-example)
    + [`MuNumber`, `MuBool`, `MuString` and `MuSymbol`](#munumber-mubool-mustring-and-musymbol)
      - [Syntax](#syntax-44)
      - [Ruby Example](#ruby-example-5)
    + [`MuTuple` and `MuList`](#mutuple-and-mulist)
      - [Syntax](#syntax-45)
  * [Patterns](#patterns)
    + [`VariablePattern`](#variablepattern)
      - [Syntax](#syntax-46)
      - [JavaScript Example](#javascript-example-1)
    + [`LiteralPattern`](#literalpattern)
      - [Syntax](#syntax-47)
      - [Example](#example-22)
    + [`InfixApplicationPattern`](#infixapplicationpattern)
      - [Syntax](#syntax-48)
        * [Caveats](#caveats-4)
    + [`ApplicationPattern`](#applicationpattern)
      - [Syntax](#syntax-49)
      - [Example](#example-23)
    + [`TuplePattern`](#tuplepattern)
      - [Syntax](#syntax-50)
      - [Example](#example-24)
    + [`ListPattern`](#listpattern)
      - [Syntax](#syntax-51)
      - [Example](#example-25)
    + [`FunctorPattern`](#functorpattern)
      - [Syntax](#syntax-52)
      - [Example](#example-26)
    + [`AsPattern`](#aspattern)
      - [Syntax](#syntax-53)
      - [Example](#example-27)
    + [`TypePattern`](#typepattern)
      - [Syntax](#syntax-54)
      - [Example](#example-28)
    + [`WildcardPattern`](#wildcardpattern)
      - [Syntax](#syntax-55)
    + [`UnionPattern`](#unionpattern)
      - [Syntax](#syntax-56)
    + [`OtherPattern`](#otherpattern)
      - [Syntax](#syntax-57)
- [Building mulang from source](#building-mulang-from-source)
  * [Setup](#setup)
  * [Installing and creating an executable](#installing-and-creating-an-executable)
  * [Running tests](#running-tests)
  * [Watching changes](#watching-changes)
  * [Loading mulang in the REPL](#loading-mulang-in-the-repl)


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
> scopedList returnsNull ["bar", "foo"] e
True
> scopedList returnsNull ["bar", "baz"] e
False
```

But instead of asking one by one, we could use `detect` :

```haskell
> detect returnsNull e
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
| `hasWrongCaseIdentifiers`         |  any               | whether an identifier does not match the domain language's case style
| `implements`                      |  object oriented   | is the given interface implemented?
| `includes`                        |  object oriented   | is a given mixins included?
| `inherits`                        |  object oriented   | is a given class declared as superclass? - alias of `declaresSuperclass`
| `instantiates`                    |  object oriented   | is the given class instantiated?
| `isLongCode`                      |  any               | has the code long sequences of statements?
| `overridesEqualsOrHashButNotBoth` |  object oriented   | does a given class override equals but not hash? or hash but not equals?
| `raises`                          |  any               | is the given _exception type_ raised?
| `rescues`                         |  any               | is the given _exception type_ rescued?
| `returnsNull`                     |
| `uses`                            |  any               | is there any reference to the given element?
| `usesAnonymousVariable`           |
| `usesComposition`                 |
| `usesComprehensions`              |
| `usesConditional`                 |
| `usesCut`                         |  logic             | is the logic `!` consult used?
| `usesExceptionHandling`           |  any               | is any _exception_ handlded?
| `usesExceptions`                  |  any               | is any _exception_ raised?
| `usesFail`                        |  logic             | is the logic `fail` consult used?
| `usesFindall`                     |  logic             | is the logic `findall` consult used?
| `usesForall`                      |  logic             | is the logic `forall` consult used?
| `usesGuards`                      |
| `usesIf`                          |  any               | is an `if` control structure used?
| `usesInheritance`                 |  object oriented   | is any superclass explicitly declared?
| `usesLambda`                      |
| `usesMixins`                      |  object oriented   | is any mixins explicitly included?
| `usesNot`                         |
| `usesPatternMatching`             |
| `usesRepeat`                      |
| `usesSwitch`                      |
| `usesUnificationOperator`         |  logic             | is the logic unification operator `=` used?
| `usesWhile`                       |  imperative        | is a `while` control structure used?


# Supported languages

Mulang is an universal tool which can work with many different programming languages. it natively supports:

  * JS (ES5)
  * Java
  * Haskell
  * Prolog

In addition, through external tools, it offers support for the following languages:

  * Ruby, using [mulang-ruby](https://github.com/mumuki/mulang-ruby)
  * Python, using [mulang-python](https://github.com/mumuki/mulang-python)
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
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : ":Intransitive:x",
            "inspection" : "Uses:*"
         }
      ],
      "smellsSet" : { "tag" : "NoSmells" }
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
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "smellsSet" : { "tag" : "NoSmells" },
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
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return x + y; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : { "tag" : "NoSmells" },
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
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y { return x + y; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : { "tag" : "NoSmells" },
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
      "tag" : "MulangSample",
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
      "smellsSet" : {
        "tag" : "NoSmells"
      },
      "signatureAnalysisType" : {
         "tag" : "StyledSignatures",
         "style" : "HaskellStyle"
      },
      "expectations" : []
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
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "tag" : "NoSmells",
        "include" : [
          "ReturnsNull",
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
         "inspection" : "ReturnsNull"
      }
   ]
}
```

### With Smell Analysis, by exclusion

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : {
        "tag" : "AllSmells",
        "exclude" : [
          "ReturnsNull"
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
      "tag" : "CodeSample",
      "language" : "Prolog",
      "content" : "son(Parent, Son):-parentOf(Son, Parent).parentOf(bart, homer)."
   },
   "spec" : {
      "expectations" : [],
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
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
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
         "inspection" : "ReturnsNull",
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
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "expectations" : [],
      "smellsSet" : { "tag" : "NoSmells" },
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
                        "tag" : "MuNull"
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


# Mulang AST spec

In this section, we will get into the technical details of the Mulang AST. It is built around 4 core elements:

* [Expressions](#expressions)
* [Patterns](#patterns)
* Equations
* Generators

All the AST elements fall within any of this 4 categories.

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


### `TypeAlias`

> A `TypeAlias` represents a synonym for a type, like the `type` declaration in Haskell and Scala or C's `typedef`.
> It is a typical statically typed functional programming feature.

#### Syntax

```haskell
(TypeAlias Identifier)
```

#### Haskell Example

```haskell
type Point = (Point, Int)
```

```haskell
(TypeAlias "Point")
```

#### Caveats

Currently, the `TypeAlias` expression does not hold information about the aliased type.

### TypeSignature

> A `TypeSignature` represents an explicit type annotation for a computation,
> variable or constant, as you can find in Java or Haskell.

#### Syntax

```haskell
(TypeSignature Identifier [Identifier] Identifier)
```

#### Haskell Example

```haskell
name :: String
```

```haskell
(TypeSignature "name" [] "String")
```

#### Java Example

In Java, as in most typed C-like languages, type signature and variable declartions are bound. This means that, for example, a local variable declaration will produce both a `TypeSignature` and a `Variable` expression.

```java
String name;
```

```haskell
(TypeSignature "name" [] "String")
```

#### Caveats

`TypeSignature`s of zero-args computation and variables are identical.

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
(EntryPoint "main" MuNull)
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
                (UnguardedBody MuNull))),
  (HashMethod (Equation
              []
              (UnguardedBody MuNull)))]

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
(Class "Bird" (Just "Animal") MuNull)
```

#### Java Examples

```java
public class Bird extends Animal {}
```

```haskell
(Class "Bird" (Just "Animal") MuNull)
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

### `Comprehension`

#### Syntax

```haskell
(Comprehension Expression [ComprehensionStatement])
```

#### Caveats

`Comprehension`s are going to be deprecated and replaced by new `ForStatement`. See [This PR](https://github.com/mumuki/mulang/pull/98).

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

### `MuNull`

> Used as a placeholder for empty bodies.

#### Syntax

```haskell
(MuNull)
```

### `MuNil`

> Generic nothing value literal - `nil`, `null`, `()` or `unit`.

#### Syntax

```haskell
(MuNil)
```

#### Example

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
(MuObject MuNull)
(MuObject (Attribute "foo" (MuNumber 1)))
(MuObject (Sequence [
            (Attribute "foo" (MuNumber 1)),
            (Attribute "bar" (MuNumber 2))]))
```

### `MuNumber`, `MuBool`, `MuString` and `MuSymbol`

> Generic number, boolean, string and symbol (atoms) literals

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
      (UnguardedBody MuNull))])
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

```
stack ghci
```
