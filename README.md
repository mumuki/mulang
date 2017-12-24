[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

Mulang
======
> A universal, multi-language, multi-paradigm code analyzer

Mulang is three different  - but thighly related - things:

  * an intermediate language, sometimes refered as the **Mulang AST**;
  * a command line tool for analysing the Mulang AST and some popular languages by transforming to it
  * a Haskell composable combinators library for analysing the Mulang AST;

# Table of contents

- [Combinators Crash course](#combinators-crash-course)
  * [Inspections and Inspection Combinators](#inspections-and-inspection-combinators)
  * [Identifier predicates](#identifier-predicates)
  * [Detections](#detections)
- [Supported inspections](#supported-inspections)
- [Supported languages](#supported-languages)
- [Command Line Tool](#command-line-tool)
    + [With intransitive expectations](#with-intransitive-expectations)
    + [With unscoped expectations](#with-unscoped-expectations)
    + [With signature analysis](#with-signature-analysis)
    + [With broken input](#with-broken-input)
    + [With AST as input](#with-ast-as-input)
    + [With Smell Analysis, by inclusion](#with-smell-analysis-by-inclusion)
    + [With Smell Analysis, by exclusion](#with-smell-analysis-by-exclusion)
    + [With expressiveness smells](#with-expressiveness-smells)
    + [With Intermediate Language Generation](#with-intermediate-language-generation)
  * [Expectations, Intermediate Langauge, Signatures and Smells](#expectations-intermediate-langauge-signatures-and-smells)
  * [Building mulang from source](#building-mulang-from-source)
    + [Setup](#setup)
    + [Installing and creating an executable](#installing-and-creating-an-executable)
    + [Running tests](#running-tests)
    + [Watching changes](#watching-changes)
    + [Loading mulang in the REPL](#loading-mulang-in-the-repl)
- [The AST spec](#the-ast-spec)
  * [Expressions](#expressions)
    + [Record](#record)
      - [Syntax](#syntax)
      - [Semantics](#semantics)
      - [C Example](#c-example)
      - [Caveats](#caveats)
    + [TypeAlias](#typealias)
      - [Syntax](#syntax-1)
      - [Semantics](#semantics-1)
      - [Haskell Example](#haskell-example)
      - [Caveats](#caveats-1)
    + [TypeSignature](#typesignature)
      - [Syntax](#syntax-2)
      - [Semantics](#semantics-2)
      - [Haskell Example](#haskell-example-1)
      - [Java Example](#java-example)
      - [Caveats](#caveats-2)
    + [`EntryPoint`](#entrypoint)
      - [Syntax](#syntax-3)
      - [Semantics](#semantics-3)
      - [Java Example](#java-example-1)
    + [`Function`](#function)
      - [Syntax](#syntax-4)
      - [Semantics](#semantics-4)
      - [Example](#example)
    + [`Procedure`](#procedure)
      - [Syntax](#syntax-5)
      - [Semantics](#semantics-5)
      - [Example](#example-1)
    + [`Method`](#method)
      - [Syntax](#syntax-6)
      - [Semantics](#semantics-6)
    + [`EqualMethod` and `HashMethod`](#equalmethod-and-hashmethod)
      - [Syntax](#syntax-7)
      - [Semantics](#semantics-7)
      - [Example](#example-2)
    + [`Variable`](#variable)
      - [Syntax](#syntax-8)
      - [Semantics](#semantics-8)
    + [`Assignment`](#assignment)
      - [Syntax](#syntax-9)
      - [Semantics](#semantics-9)
    + [`Attribute`](#attribute)
      - [Syntax](#syntax-10)
      - [Semantics](#semantics-10)
      - [Example](#example-3)
    + [`Object`](#object)
      - [Syntax](#syntax-11)
      - [Semantics](#semantics-11)
      - [Example](#example-4)
      - [Example](#example-5)
    + [`Class`](#class)
      - [Syntax](#syntax-12)
      - [Semantics](#semantics-12)
      - [Example](#example-6)
      - [Example](#example-7)
    + [`Enumeration`](#enumeration)
      - [Syntax](#syntax-13)
      - [Semantics](#semantics-13)
      - [Example](#example-8)
    + [`Interface`](#interface)
      - [Syntax](#syntax-14)
      - [Semantics](#semantics-14)
      - [Example](#example-9)
      - [Example](#example-10)
    + [`Rule`](#rule)
      - [Syntax](#syntax-15)
      - [Semantics](#semantics-15)
      - [Example](#example-11)
    + [`Fact`](#fact)
      - [Syntax](#syntax-16)
      - [Semantics](#semantics-16)
      - [Example](#example-12)
    + [`Exist`](#exist)
      - [Syntax](#syntax-17)
      - [Semantics](#semantics-17)
      - [Example](#example-13)
    + [`Not`](#not)
      - [Syntax](#syntax-18)
      - [Semantics](#semantics-18)
      - [Example](#example-14)
    + [`Findall`](#findall)
      - [Syntax](#syntax-19)
      - [Semantics](#semantics-19)
      - [Example](#example-15)
    + [`Forall`](#forall)
      - [Syntax](#syntax-20)
      - [Semantics](#semantics-20)
      - [Example](#example-16)
    + [`Reference`](#reference)
      - [Syntax](#syntax-21)
      - [Semantics](#semantics-21)
      - [Example](#example-17)
    + [`Application`](#application)
      - [Syntax](#syntax-22)
      - [Semantics](#semantics-22)
      - [Example](#example-18)
    + [`Send`](#send)
      - [Syntax](#syntax-23)
      - [Semantics](#semantics-23)
      - [Example](#example-19)
    + [`New`](#new)
      - [Syntax](#syntax-24)
      - [Semantics](#semantics-24)
      - [Example](#example-20)
    + [`Implement`](#implement)
      - [Syntax](#syntax-25)
      - [Semantics](#semantics-25)
      - [Example](#example-21)
    + [`Include`](#include)
      - [Syntax](#syntax-26)
      - [Semantics](#semantics-26)
      - [Example](#example-22)
    + [`Lambda`](#lambda)
      - [Syntax](#syntax-27)
      - [Semantics](#semantics-27)
    + [`If`](#if)
      - [Syntax](#syntax-28)
      - [Semantics](#semantics-28)
    + [`Return`](#return)
      - [Syntax](#syntax-29)
      - [Semantics](#semantics-29)
    + [`While`](#while)
      - [Syntax](#syntax-30)
      - [Semantics](#semantics-30)
      - [Example](#example-23)
    + [`Repeat`](#repeat)
      - [Syntax](#syntax-31)
      - [Semantics](#semantics-31)
      - [Example](#example-24)
    + [`Match`](#match)
      - [Syntax](#syntax-32)
      - [Semantics](#semantics-32)
    + [`Switch`](#switch)
      - [Syntax](#syntax-33)
      - [Semantics](#semantics-33)
    + [`Try`](#try)
      - [Syntax](#syntax-34)
      - [Semantics](#semantics-34)
      - [Example](#example-25)
    + [`Raise`](#raise)
      - [Syntax](#syntax-35)
      - [Semantics](#semantics-35)
      - [Example](#example-26)
    + [`Print`](#print)
      - [Syntax](#syntax-36)
      - [Semantics](#semantics-36)
      - [Example](#example-27)
    + [`Comprehension`](#comprehension)
      - [Syntax](#syntax-37)
      - [Semantics](#semantics-37)
    + [`Sequence`](#sequence)
      - [Syntax](#syntax-38)
      - [Semantics](#semantics-38)
      - [Example](#example-28)
    + [`Other`](#other)
      - [Syntax](#syntax-39)
      - [Semantics](#semantics-39)
    + [`Equal`](#equal)
      - [Syntax](#syntax-40)
      - [Semantics](#semantics-40)
    + [`NotEqual`](#notequal)
      - [Syntax](#syntax-41)
      - [Semantics](#semantics-41)
    + [`Self`](#self)
      - [Syntax](#syntax-42)
      - [Semantics](#semantics-42)
    + [`MuNull`](#munull)
      - [Syntax](#syntax-43)
      - [Semantics](#semantics-43)
      - [Example](#example-29)
    + [`MuObject`](#muobject)
      - [Syntax](#syntax-44)
      - [Semantics](#semantics-44)
      - [Example](#example-30)
    + [`MuNumber`](#munumber)
      - [Syntax](#syntax-45)
      - [Semantics](#semantics-45)
      - [Example](#example-31)
    + [`MuBool`](#mubool)
      - [Syntax](#syntax-46)
      - [Semantics](#semantics-46)
      - [Example](#example-32)
    + [`MuString`](#mustring)
      - [Syntax](#syntax-47)
      - [Semantics](#semantics-47)
      - [Example](#example-33)
    + [`MuSymbol`](#musymbol)
      - [Syntax](#syntax-48)
      - [Semantics](#semantics-48)
      - [Example](#example-34)
    + [`MuTuple`](#mutuple)
      - [Syntax](#syntax-49)
      - [Semantics](#semantics-49)
    + [`MuList`](#mulist)
      - [Syntax](#syntax-50)
      - [Semantics](#semantics-50)

# Combinators Crash course

## Inspections and Inspection Combinators

Better than explaining what Mulang is, let's see what can do it for you. Let's suppose we have the following JS code...

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

Now the magic begins. We want to know if the code expression _uses_ - that is, contains any reference to - a certain identifier. Such identifier could be a variable, function, or anything that has a name:

```haskell
> uses (named "buenosAires") e
True
> uses (named "rosario") e
False
```

`uses (named "buenosAires")` is our first _inspection_: a function that takes a Mulang AST and answers a boolan question about it. That _seems_ easy, but just in case you are wondering: no, Mulang doesn't perform a `string.contains` or something like that :stuck_out_tongue: :

```haskell
> uses (named "buenos") e
False
```

So let's ask something more interesting - does `aPlace` _use_ the identifier `buenosAires`?

```haskell
> scoped (uses (named "buenosAires")) "aPlace"  e
True
```

Here we have used the our first _inspection combinator_, a function that takes an inspection - `uses (named "buenosAires")` - and returns a new one that is more powerful. In this case, `scoped` is capable of restricting the analysis to the given context - the `aPlace` identifier.


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

Does `aBird` use `buenosAires`?

```haskell
> scoped (uses (named "buenosAires")) "aBird"  e
False
```

Oh, wait there! We know, it is true that it does not use **exactly** that variable, but come on, `aPlace` does use `buenosAires`! Wouldn't it be sweet to be transitive?

```haskell
> transitive (uses (named "buenosAires")) "aBird"  e
True
```

Here we can see another _inspections combinator_: `transitive`, which inspects the given context and all the contexts that are refered from it.

Contexts can be nested, too: for example, if you want to know whether `aBird.position` _uses_ `aPlace` - ignoring that `weight` attribute:

```haskell
> scopedList (uses (named "aPlace")) ["aBird", "position"]  e
True
> scopedList (uses (named "aPlace")) ["aBird", "weight"]  e
False
```

Nice, we know. But not very awesome, it only can tell you if you are using a _identifier_, right? Eeer. Good news, it can tell you much much much more things. See the supported inspections list.

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

You can also use Mulang from the Command Line, without having to interact with Haskell code. Let's see some samples:

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


## Expectations, Intermediate Langauge, Signatures and Smells

Mulang CLI can do four different kinds of analysis:

* **Expectation analysis**: you can provide an expression - called `inspection` - that will be tested against the provied program. Expectations answer questions like: _does the function X call the function Y?_ or _does the program use if's?_. They can be expressed with the following simple DSL:
  * Simple inspections, like `HasIf` or `DeclaresClass`
  * Negated inspections, like `Not:HasIf`
  * Targeted inspections, like `DeclaresClass:Golondrina`. You can specify targets the following ways:
    * Exact matches:  `DeclaresClass:=Golondrina` or simply `DeclaresClass:Golondrina`
    * Approximate matches: `DeclaresClass:~Golondrina`
    * Any matches: `DeclaresClass:*` or simply `DeclaresClass`
    * Except matches: `Declares:^Foo` - wich means that will match any declaration that is not `Foo`
    * Any-Of matches: `Declares:[Foo|IFoo|AbstractFoo]` - which means that will match any declaration of `Foo`, `IFoo` or `AbstractFoo`
* **Intermediate Language**: Mulang Command Line Tool can generate the Mulang AST for a given source code.
* **Signature analysis**: report the signatures of the computations present in source code.
* **Smell analysis**: instead of asking explcit questions to the program, the smells analysis implicitly runs specific inspections - that denote bad code - in orden to know if any of them is matched.

## Building mulang from source

### Setup

To generate `mulang` executable, you have to build the project using [stack](https://haskellstack.org):

1. Install stack: `wget -qO- https://get.haskellstack.org/ | sh`
2. Go to the mulang project directory and setup it: `stack setup`
3. Build the project: `stack build`

### Installing and creating an executable


```bash
$ stack install
$ mulang
```

That will generate a `mulang` executable in the folder `~/.local/bin`.

### Running tests

```bash
$ stack test --fast
```

### Watching changes


```bash
$ stack test --fast --file-watch
```

### Loading mulang in the REPL

```
stack ghci
```


# The AST spec

In this last section, we will get into the technical details of the Mulang AST. It is built around 4 core elements:

* Expressions
* Patterns
* Equations
* Generators

All the AST elements fall within any of this 4 categories.

## Expressions

Expressions are the most important element kind, since contain most of the information of a Mulang program and are always the root element of it. In fact, this implementation does not contain an `AST` or `Program` datatype - it is instead types as `Expression`.

Expression in Mulang model what you will normally spec in a language as a expression, that is something that holds a value and a type. For example, `4 + 5` and `[2, 3].toString()` are typical expresion.

However, Mulang extends this concept to most kind of elements in a program, regadless they are have an actual value in the original language. For example, class declarations and while statements are modeled as expression, although in many languages they aren't.

As a rule of thumb if something is or can be represented as an statement, declararion or expression, the it is modeled as `Expression` in Mulang AST.

### Record

#### Syntax

```haskell
(Record Identifier)
```

#### Semantics

A `Record` represents a record, data or struct declaration, as found in most procedural and functional languages, like the C-like `struct` declaration

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


### TypeAlias

#### Syntax

```haskell
(TypeAlias Identifier)
```

#### Semantics

A `TypeAlias` represents a synonym for a type, like the `type` declaration in Haskell and Scala or C's `typedef`. It is a typical statically typed functional programming feature.

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

#### Syntax

```haskell
(TypeSignature Identifier [Identifier] Identifier)
```

#### Semantics

A `TypeSignature` represents an explicit type annotation for a computation, variable or constant, as you can find in Java or Haskell.

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

#### Syntax

```haskell
(EntryPoint Identifier Expression)
```

#### Semantics

Entry point with its name and body

#### Java Example

```java
public static main(String[] args) {}
```

```haskell
(EntryPoint "main" MuNull)
```

### `Function`

#### Syntax

```haskell
(Function Identifier [Equation])
```

#### Semantics

Functional / Imperative programming function declaration.
It is is composed by an identifier and one or more equations

#### Example

### `Procedure`

#### Syntax

```haskell
(Procedure Identifier [Equation])
```

#### Semantics

Imperative programming procedure declaration. It is composed by a name and one or more equations

#### Example

### `Method`

#### Syntax

```haskell
(Method Identifier [Equation])
```

#### Semantics

### `EqualMethod` and `HashMethod`

#### Syntax

```haskell
(EqualMethod [Equation])
(HashMethod [Equation])
```

#### Semantics

Declaration of custom equivalance and _hash code_ operations. `EqualMethod` typically corresponds to `equals` or `==` method declarations, while `HashMethod`, typically corresponds to `hash` or `hashCode`- like methods.

#### Example

### `Variable`

#### Syntax

```haskell
(Variable Identifier Expression)
```

#### Semantics

### `Assignment`

#### Syntax

```haskell
(Assignment Identifier Expression)
```

#### Semantics

### `Attribute`

#### Syntax

```haskell
(Attribute Identifier Expression)
```

#### Semantics

Object oriented programming attribute declaration, composed by an identifier and an initializer

#### Example

### `Object`

#### Syntax

```haskell
(Object Identifier Expression)
```

#### Semantics

Object oriented programming global, named object declaration,

#### Example

composed by a name and a body

#### Example

### `Class`

#### Syntax

```haskell
(Class Identifier (Maybe Identifier) Expression)
```

#### Semantics

Object oriented programming global, class declaration,

#### Example

composed by a name, an optional superclass, implemented interfaces and a body

#### Example

### `Enumeration`

#### Syntax

```haskell
(Enumeration Identifier [Identifier])
```

#### Semantics

Imperative named enumeration of values

#### Example

### `Interface`

#### Syntax

```haskell
(Interface Identifier [Identifier] Expression)
```

#### Semantics

Object oriented programming global interface or contract declaration,

#### Example

composed by a name, superinterfaces and a body

#### Example

### `Rule`

#### Syntax

```haskell
(Rule Identifier [Pattern] [Expression])
```

#### Semantics

Logic programming declaration of a fact, composed by the rue name, rule arguments, and rule body

#### Example

### `Fact`

#### Syntax

```haskell
(Fact Identifier [Pattern])
```

#### Semantics

Logic programming declaration of a fact , composed by the fact name and fact arguments

#### Example

### `Exist`

#### Syntax

```haskell
(Exist Identifier [Pattern])
```

#### Semantics

Logic programming existential cuantification / consult

#### Example

### `Not`

#### Syntax

```haskell
(Not Expression)
```

#### Semantics

Logic programming negation

#### Example

### `Findall`

#### Syntax

```haskell
(Findall Expression Expression Expression)
```

#### Semantics

Logic programming findall

#### Example

### `Forall`

#### Syntax

```haskell
(Forall Expression Expression)
```

#### Semantics

Logic programming universal cuantification

#### Example

### `Reference`

#### Syntax

```haskell
(Reference Identifier)
```

#### Semantics

Generic variable

#### Example

### `Application`

#### Syntax

```haskell
(Application Expression [Expression])
```

#### Semantics

Generic, non-curried application of a function or procedure, composed by the applied element itself, and the application arguments

#### Example

### `Send`

#### Syntax

```haskell
(Send Expression Expression [Expression])
```

#### Semantics

Object oriented programming message send, composed by the reciever, selector and arguments

#### Example

### `New`

#### Syntax

```haskell
(New Identifier [Expression])
```

#### Semantics

Object oriented instantiation, composed by the class reference and instantiation arguments

#### Example

### `Implement`

#### Syntax

```haskell
(Implement Identifier)
```

#### Semantics

Object oriented instantiation, interface implementation

#### Example

### `Include`

#### Syntax

```haskell
(Include Identifier)
```

#### Semantics

Object oriented instantiation, mixin inclusion

#### Example

### `Lambda`

#### Syntax

```haskell
(Lambda [Pattern] Expression)
```

#### Semantics

### `If`

#### Syntax

```haskell
(If Expression Expression Expression)
```

#### Semantics

### `Return`

#### Syntax

```haskell
(Return Expression)
```

#### Semantics

### `While`

#### Syntax

```haskell
(While Expression Expression)
```

#### Semantics

Imperative programming conditional repetition control structure, composed by a condition and a body

#### Example

### `Repeat`

#### Syntax

```haskell
(Repeat Expression Expression)
```

#### Semantics

Imperative programming fixed repetition control structure, composed by a repetition count expression, and a body

#### Example

### `Match`

#### Syntax

```haskell
(Match Expression [Equation])
```

#### Semantics

### `Switch`

#### Syntax

```haskell
(Switch Expression [(Expression, Expression)])
```

#### Semantics

### `Try`

#### Syntax

```haskell
(Try Expression [(Pattern, Expression)] Expression)
```

#### Semantics

Generic try expression, composed by a body, a list of exception-handling patterns and statments, and a finally expression

#### Example

### `Raise`

#### Syntax

```haskell
(Raise Expression)
```

#### Semantics

Generic raise expression, like a throw or raise statament, composed by the raised expression

#### Example

### `Print`

#### Syntax

```haskell
(Print Expression)
```

#### Semantics

Generic print expression

#### Example

### `Comprehension`

#### Syntax

```haskell
(Comprehension Expression [ComprehensionStatement])
```

#### Semantics

### `Sequence`

#### Syntax

```haskell
(Sequence [Expression])
```

#### Semantics

Generic sequence of statements

#### Example

### `Other`

#### Syntax

```haskell
(Other)
```

#### Semantics

### `Equal`

#### Syntax

```haskell
(Equal)
```

#### Semantics

### `NotEqual`

#### Syntax

```haskell
(NotEqual)
```

#### Semantics

### `Self`

#### Syntax

```haskell
(Self)
```

#### Semantics

### `MuNull`

#### Syntax

```haskell
(MuNull)
```

#### Semantics

Generic nothing value literal - nil, null, undefined or unit

#### Example

### `MuObject`

#### Syntax

```haskell
(MuObject Expression)
```

#### Semantics

Object oriented unnamed object literal

#### Example

### `MuNumber`

#### Syntax

```haskell
(MuNumber Double)
```

#### Semantics

Generic number literal

#### Example

### `MuBool`

#### Syntax

```haskell
(MuBool Bool)
```

#### Semantics

Generic boolean literal

#### Example

### `MuString`

#### Syntax

```haskell
(MuString String)
```

#### Semantics

Generic string literal

#### Example

### `MuSymbol`

#### Syntax

```haskell
(MuSymbol String)
```

#### Semantics

Generic symbol/atom literal

#### Example

### `MuTuple`

#### Syntax

```haskell
(MuTuple [Expression])
```

#### Semantics

### `MuList`

#### Syntax

```haskell
(MuList [Expression])
```

#### Semantics
