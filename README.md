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


## The expectations DSL

In order to pass expectations to the Command Line Tool, you must use a simple DSL that builds the inspections for you.

| Kind              | DSL Sample                         | Haskell Combinators Sample
|-------------------|------------------------------------|----------------------------
| Basic             | `* UsesIf`                         | `usesIf`
| Negated           | `* Not:UsesWhile`                  | `(negative usesWhile)`
| Predicated        | `* DeclaresClass:Foo`              | `(declaresClass (named "Foo"))`
|                   | `* DeclaresClass:=Foo`             | `(declaresClass (named "Foo"))`
|                   | `* DeclaresClass:~Foo`             | `(declaresClass (like "Foo"))`
|                   | `* DeclaresClass:^Foo`             | `(declaresClass (except "Foo"))`
|                   | `* DeclaresClass:*`                | `(declaresClass anyone)`
|                   | `* DeclaresClass`                  | `(declaresClass anyone)`
| Matching Literals | `* Calls:*:WithChar:'a':WithTrue`  | `(callsMatching (withEvery [isChar 'a', isBool True]) anyone)`
|                   | `* Calls:*:WithNil`                | `(callsMatching (withEvery [isNil]) anyone)`
|                   | `* Returns:WithNumber:1`           | `(returnsMatching (withEvery [isNumber 1]) anyone)`
|                   | `* Returns:WithString:"foo"`       | `(returnsMatching (withEvery [isString "foo"]) anyone)`
|                   | `* Returns:WithSymbol:"foo"`       | `(returnsMatching (withEvery [isSymbol "foo"]) anyone)`
| Transitive        | `foo UsesLambda`                   | `(transitive usesLambda "foo")`
| Scoped            | `Intransitive:foo UsesIf`          | `(scoped usesIf "foo")`
| Scoped List       | `foo.bar UsesIf`                   | `(scopedList usesIf ["foo", "bar"])`


## Examples

Let's see some usage samples:

### With intransitive expectations

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = z + 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : "Intransitive:x",
            "inspection" : "Uses:z"
         }
      ]
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [
      {
         "expectation" : {
            "binding" : "Intransitive:x",
            "inspection" : "Uses:z"
         },
         "result" : true
      }
   ],
   "smells" : [],
   "intermediateLanguage" : null,
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
      "tag" : "CodeSample",
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
      "tag" : "CodeSample",
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
      "tag" : "CodeSample",
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
      "tag" : "CodeSample",
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


# Customs Expectations and the Expectations Definition Language

Mulang accepts custom-made expectations expressed in an Expectations Definition Language - EDL - which allows to build complex
expectations by combining existing inspections.


# Contributors

 * Franco Bulgarelli @flbulgarelli [Mumuki](@mumuki)
 * Julian Berbel Alt @julian-berbel @ [Mumuki](@mumuki)
 * Federico Lochbaum @FedeLochbaum @ [UNQ](http://www.unq.edu.ar/)
 * Lucas Traverso @ludat @ [10Pines](@10pines)
