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
      "expectations" : [],
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
