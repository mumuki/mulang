[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

Mulang
======
> The Universal, Multi Language, Multi Paradigm code analyzer

## Getting Started

Better than explaining what Mulang is, let's see what can do it for you.

Let's start simple - we have the following JS expression:

```javascript
var pepita = {lugar: bsAs1, peso: 20};
var bsAs1 = bsAs
```

We want to recognize some code patterns on it, so we will first load the expression into Mulang:

```
$ ghci
> :m Language.Mulang.All
> let e = js "var pepita = {lugar: bsAs1, peso: 20}; var bsAs1 = bsAs"
```

Now the magic begins. We want to know if the code expression uses a certain identifier - that could be a variable, function, or anything that has a name:

```haskell
> uses (named "bsAs") e
True
> uses (named "rosario") e
False
```

That _seems_ easy, but just in case you are wondering: no, Mulang doesn't perform a `string.contains` or something like that :stuck_out_tongue: :

```haskell
> uses (named "bs") e
False
```

So let's ask something more interesting - does `bsAs1` use the identifier `bsAs`?

```haskell
> scoped (uses (named "bsAs")) "bsAs1"  e
True
```

And does it use `rosario`?

```haskell
> scoped (uses (named "rosario")) "bsAs1"  e
False
```

What about the object `pepita`? Does it use `bsAs1` or `rosario`?

```haskell
> scoped (uses (named "bsAs1")) "pepita"  e
True
> scoped (uses (named "rosario")) "pepita"  e
False
```

Does `pepita` use `bsAs`?

```haskell
> scoped (uses (named "bsAs")) "pepita"  e
False
```

Oh, wait there! We know, it is true that it does not use **exactly** that variable, but come on, `bsAs1` does use `bsAs`! Wouldn't it be sweet to be transitive?

You ask for it, you get it:

```haskell
> transitive (uses (named "bsAs")) "pepita"  e
True
```

I know what you are thinking:  now you wan't to be stricter, you want to know if `pepita.lugar` uses bsAs1 - ignoring that `peso` attribute. Piece of cake:

```haskell
> scopedList (uses (named "bsAs1")) ["pepita", "lugar"]  e
True
> scopedList (uses (named "bsAs1")) ["pepita", "peso"]  e
False
```

Nice, we know. But not very awesome, it only can tell you if you are using a _identifier_, right? Eeer. Good news, it can tell you much much much more things:

1. `assigns`: **any paradigm** is the given variable or attribute assigned?
1. `calls`: **any paradigm** is the given method, function or procedure called?
1. `declares`: **any paradigm** is the given element declared?
1. `declaresAttribute`: **object oriented paradigm** is a given attribute declared?
1. `declaresClass`: **object oriented paradigm** is a given class declared?
1. `declaresComputation`: **any paradigm** that is, does the given computation  - method, predicate, function, etc - exist?
1. `declaresComputationWithArity`: **any paradigm**  that is, does the given computation have the exact given arity?
1. `declaresEntryPoint`: **any paradigm** is there a program entry point, like a `main` procedure?
1. `declaresEnumeration`: **imperative paradigm** is a given enumeration declared?
1. `declaresFact`: **logic paradigm** is a given logic fact declared?
1. `declaresFunction`: **functional/imperative paradigm** is a given function declared?
1. `declaresInterface`: **object oriented paradigm** is a given interface declared?
1. `declaresMethod`: **object oriented paradigm** is a given method declared?
1. `declaresObject`: **object oriented paradigm** is a given named object declared?
1. `declaresPredicate`: **logic paradigm** is a given rule o fact declared?
1. `declaresProcedure`: **imperative paradigm** is a given procedure declared?
1. `declaresRecursively`: **any paradigm** is a given computation declared using recusion?
1. `declaresRule`: **logic paradigm** is a given logic rule declared?
1. `declaresSuperclass`: **object oriented paradigm** is a given class declared as superclass?
1. `declaresTypeAlias`: **any paradigm** is a given type synonym declared?
1. `declaresTypeSignature`: **any paradigm** is a given computation type signature declared?
1. `declaresVariable`: **any paradigm** is a given local o global variable declared?
1. `discardsExceptions`: **any paradigm** are exceptions discarded within an empty catch block?
1. `doesConsolePrint`: **any paradigm** is there any console-print-statement like `System.out.println`, `puts` or `console.log`?
1. `doesNullTest`: **object oriented paradigm** is there a test agains a null value, like `if x == nil then puts 'is nil'`
1. `doesTypeTest`
1. `hasAssignmentReturn`
1. `hasCodeDuplication`: **any paradigm** has the given code simple literal code duplication?
1. `hasMisspelledIdentifiers`: **any paradigm** an identifier is not a domain language dictionary's word and not part of its jargon
1. `hasRedundantBooleanComparison`
1. `hasRedundantGuards`
1. `hasRedundantIf`: **any paradigm** can a combination of `if`s, `assignment`s and `return`s be replaced by a boolean expression?
1. `hasRedundantLambda`
1. `hasRedundantLocalVariableReturn`
1. `hasRedundantParameter`
1. `hasRedundantReduction`: **logic paradigm** is a is-operator used to unify individuals that don't require a reduction, like `X is 4`
1. `hasTooShortIdentifiers`: **any paradigm** whether an identifier is too short and not part of domain language's jargon
1. `hasWrongCaseIdentifiers`: **any paradigm** whether an identifier does not match the domain language's case style
1. `implements`: **object oriented paradigm** is the given interface implemented?
1. `includes`: **object oriented paradigm** is a given mixins included?
1. `inherits`: **object oriented paradigm** is a given class declared as superclass? - alias of `declaresSuperclass`
1. `instantiates`: **object oriented paradigm** is the given class instantiated?
1. `isLongCode`: **any paradigm** has the code long sequences of statements?
1. `raises`: **any paradigm** is the given _exception type_ raised?
1. `rescues`: **any paradigm** is the given _exception type_ rescued?
1. `returnsNull`
1. `uses`: **any paradigm** is there any reference to the given element?
1. `usesAnonymousVariable`
1. `usesComposition`
1. `usesComprehensions`
1. `usesConditional`
1. `usesCut`: **logic paradigm** is the logic `!` consult used?
1. `usesExceptionHandling`: **any paradigm** is any _exception_ handlded?
1. `usesExceptions`: **any paradigm** is any _exception_ raised?
1. `usesFail`: **logic paradigm** is the logic `fail` consult used?
1. `usesFindall`:  **logic paradigm** is the logic `findall` consult used?
1. `usesForall`:  **logic paradigm** is the logic `forall` consult used?
1. `usesGuards`
1. `usesIf`: **any paradigm** is an `if` control structure used?
1. `usesInheritance`: **object oriented paradigm** is any superclass explicitly declared?
1. `usesLambda`
1. `usesMixins`: **object oriented paradigm** is any mixins explicitly included?
1. `usesNot`
1. `usesPatternMatching`
1. `usesRepeat`
1. `usesSwitch`
1. `usesUnificationOperator`:  **logic paradigm** is the logic unification operator `=` used?
1. `usesWhile`: **imperative paradigm** is a `while` control structure used?

For example, let's go trickier:

Does that piece of code declare any attribute?

```haskell
> declaresAttribute anyone e
True
```

But does it declare an attribute like 'eso'?

```haskell
> declaresAttribute (like "eso") e
True
```

And does `pepita` use any if within its definition?

```haskell
> scoped usesIf "pepita" e
False
```

Does something in the following code...

```haskell
let e = js "var bar = {baz: function(){ return g }, foo: function(){ return null }}"
```

...return null?

```haskell
> returnsNull e
True
```

is it the `foo` method in `bar`? or the `baz` method?

```haskell
> scopedList returnsNull ["bar", "foo"] e
True
> scopedList returnsNull ["bar", "baz"] e
False
```

But instead of asking one by one, we could use `detect` :wink: :

```haskell
> detect returnsNull e
["bar","foo"]
```

_Which means that there are null returns within  `bar` and also within `foo`_

## An universal tool

The really awesome is here: it is an universal tool which can _potentially_ work with every programming language. it natively supports:

  * JS (ES5)
  * Java
  * Haskell
  * Prolog
  * Mulang itself, expressed as a JSON AST.

In addition, through external tools, it offers support for the following languages:

  * Ruby, using [mulang-ruby](https://github.com/mumuki/mulang-ruby)
  * Python, using [mulang-python](https://github.com/mumuki/mulang-python)
  * Gobstones, using [gs-weblang-cli](https://github.com/gobstones/gs-weblang-cli)

So in order to use it with a particular language, you have to:

* either add explicit support in this repo, or
* translate your language into one of the natively supported ones, or
* translate your language to the Mulang JSON AST

## Installing it

Mulang is just a Haskell library. You can install it though cabal.

But if you are not the Haskell inclined gal or guy - ok, I will try to forgive you - this code comes with a command line too. So you don't even have to typecheck!

## Sample CLI usage

### With intransitive expectations:

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
            "scope" : ":Intransitive:x",
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
            "scope" : ":Intransitive:x",
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
            "scope" : "*",
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
            "scope" : "*",
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
         "scope" : "foo",
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
         "minimumBindingSize" : 4,
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
         "scope" : "son"
      },
      {
         "scope" : "parentOf",
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
         "scope" : "foo"
      },
      {
         "inspection" : "HasMisspelledIdentifiers",
         "scope" : "foo"
      }
   ]
}
```

## Expectations, Signatures and Smells

Mulang CLI can do three different kinds of analysis:

* **Expectation analysis**: you can provide an expression - called `inspection` - that will be tested against the provied program. Expectations answer questions like: _does the function X call the function Y?_ or _does the program use if's?_. They can be expressed with the following simple DSL:
  * Simple inspections, like `HasIf` or `DeclaresClass`
  * Negated inspections, like `Not:HasIf`
  * Targeted inspections, like `DeclaresClass:Golondrina`. You can specify targets the following ways:
    * Exact matches:  `DeclaresClass:=Golondrina` or simply `DeclaresClass:Golondrina`
    * Approximate matches: `DeclaresClass:~Golondrina`
    * Any matches: `DeclaresClass:*` or simply `DeclaresClass`
    * Except matches: `Declares:^Foo` - wich means that will match any declaration that is not `Foo`
    * Any-Of matches: `Declares:[Foo|IFoo|AbstractFoo]` - which means that will match any declaration of `Foo`, `IFoo` or `AbstractFoo`
* **Smell analysis**: instead of asking explcit questions to the program, the smells analysis implicitly runs specific inspections - that denote bad code - in orden to know if any of them is matched.
* **Signature analysis**: report the signatures of the computations present in source code.

## Building mulang from source

### Setup

To generate `mulang` executable, you have to build the project using [stack](https://haskellstack.org):

1. Install stack: `wget -qO- https://get.haskellstack.org/ | sh`
2. Go to the mulang project directory and setup it: `stack setup`
3. Build the project: `stack build`

### Rungs

Mulang uses the `rungs` command to parse the Gobstones language - if you don't install it, Gobstones tests will fail:

```
$ wget https://github.com/gobstones/gs-weblang-cli/releases/download/v1.3.3/rungs-ubuntu64 -O rungs
$ chmod u+x rungs
$ sudo mv rungs ~/.local/bin
```

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
