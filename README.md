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

Now the magic begins. We want to know if the code expression uses a certain binding - that could be a variable, function, or anything that has a name:

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

So let's ask something more interesting - does `bsAs1` use the binding `bsAs`?

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

Nice, we know. But not very awesome, it only can tell you if you are using a _binding_, right? Eeer. Good news, it can tell you much much much more things:

* `declaresMethod`: **objects paradigm** is a given method declared?
* `declaresAttribute`: **objects paradigm** is a given attribute declared?
* `declaresFunction`: **functional/imperative paradigm** is a given function declared?
* `declaresTypeSignature`: **any paradigm** is a given computation type signature declared?
* `declaresTypeAlias`: **any paradigm** is a given type synonym declared?
* `declaresRecursively`: **any paradigm** is a given computation declared using recusion?
* `declaresComputation`: **any paradigm** that is, does the given computation  - method, predicate, function, etc - exist?
* `declaresComputationWithArity`: **any paradigm** that is, does the given computation arity match the given criteria
* `declaresComputationWithExactArity`: **any paradigm** that is, does the given computation have the exact given arity?
* `declaresRule`: **logic paradigm** is a given logic rule declared?
* `declaresFact`: **logic paradigm** is a given logic fact declared?
* `declaresPredicate`: **logic paradigm** is a given rule o fact declared?
* `usesIf`
* `usesWhile`
* `usesLambda`
* `usesGuards`
* `usesComposition`
* `usesComprehensions`
* `usesAnonymousVariable`
* `usesUnifyOperator`
* `hasRedundantIf`
* `hasRedundantGuards`
* `hasRedundantParameter`
* `hasRedundantLambda`
* `hasRedundantBooleanComparison`
* `hasRedundantLocalVariableReturn`
* `hasAssignmentReturn`
* `doesTypeTest`
* `doesNullTest`
* `returnsNull`
* `isLongCode`: **any code** has the code long sequences of statements?
* `hasCodeDuplication`: **any paradigm** has the given code simple literal code duplication?

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
  * Haskell
  * Prolog
  * Gobstones
  * Mulang itself, expressed as a JSON AST. 

So in order to use it with a particular language, you have to:

* either add explicit support in this repo, or
* translate your language into one of the natively supported ones, or
* translate your language to the Mulang JSON AST

## Installing it

Mulang is just a Haskell library. You can install it though cabal.

But if you are not the Haskell inclined gal or guy - ok, I will try to forgive you - this code comes with a command line too. So you don't even have to typecheck!

Sample CLI usage...

...with advanced expectations:
```
$ mulang '{"expectations":[{"tag":"Advanced","subject":["x"],"transitive":false,"negated":false,"object":{"tag":"Anyone","contents":[]},"verb":"uses"}],"code":{"content":"x = 1","language":"Haskell"}}'
{"results":[{"result":false,"expectation":{"subject":["x"],"tag":"Advanced","transitive":false,"negated":false,"object":{"tag":"Anyone","contents":[]},"verb":"uses"}}],"smells":[]}
```

...and with basic expectations:
```
$ mulang '{"expectations":[{"tag":"Basic","binding":"x","inspection":"HasBinding"}],"code":{"content":"x = 1","language":"Haskell"}}'
{"results":[{"result":true,"expectation":{"tag":"Basic","inspection":"HasBinding","binding":"x"}}],"smells":[]}
```

  
## Expectations and Smells  

Mulang CLI can do two different kinds of analysis: 

* **Expectation analysis**: you can provide an expression - called `inspection` - that will be tested against the provied program. Expectations answer questions like: _does the function X call the function Y?_ or _does the program use if's?_. It comes in two flavors: 
     * **basic expectations**: are composed by a binding and an inspection 
     * **advanced expectations**: are composed by
       * subject
       * verb
       * object
       * flags
* **Smell analysis**: instead of asking explcit questions to the program, the smells analysis implicitly runs specific inspections - that denote bad code - in orden to know if any of them is matched. 


## Building mulang from source

To generate `mulang` executable, you have to build the project using [cabal](https://www.haskell.org/cabal/).

Make sure you have `cabal` command installed. You can install it by doing `sudo apt-get install cabal-install` or by downloading binaries from [here](https://www.haskell.org/cabal/download.html).

Important:  cabal version must be **higher than 1.18**. Otherwise, sandboxes won't work.

You will need to install [ghc 7.10.3](https://www.haskell.org/ghc/), [alex](https://www.haskell.org/alex/) and [happy](https://www.haskell.org/happy/), two executables that Mulang uses:

```
sudo apt-get install alex
sudo apt-get install happy
```

Mulang uses the rungs` command to parse the Gobstones language - if you don't install it, Gobstones tests will fail:

```
wget https://github.com/gobstones/gs-weblang-cli/releases/download/v1.3.3/rungs-ubuntu64 -O rungs
chmod u+x rungs
sudo mv rungs /usr/bin/rungs
```

### Before start

## Installing and creating an executable

The following times, you just need to install project dependencies and build:

```
cabal sandbox init
cabal install --only-dependencies --enable-tests --force-reinstalls
cabal build
```

That will generate a `mulang` executable in the folder `dist/build/mulang`.

### Running tests

```
cabal test
```

### Loading mulang in the REPL

```
cabal repl
```

And then, inside the REPL, do:

```
:m Language.Mulang.All
```
