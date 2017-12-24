[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

Mulang
======
> A universal, multi-language, multi-paradigm code analyzer

# What Mulang is

Mulang itself is three different but thighly related things:

  * an intermediate language, sometimes refered as the **Mulang AST**;
  * a command line tool for analysing the Mulang AST and some popular languages by transforming to it
  * a Haskell composable combinators library for analysing the Mulang AST;

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


