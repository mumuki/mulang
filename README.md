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
* `declaresWithArity`: **any paradigm** that is, does the given method, function, procedure, etc have the given amount of parameters?
* `declaresRule`: **logic paradigm** is a given logic rule declared?
* `declaresFact`: **logic paradigm** is a given logic fact declared?
* `declaresPredicate`: **logic paradigm** is a given rule o fact declared?
* `usesIf`
* `usesWhile`
* `usesLambda`
* `usesGuards`
* `usesComposition`
* `usesComprehensions`
* `usesAnnonymousVariable`
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

And the really awesome is here: it works for every - yes, we said it - language on the world, because:

  * Mulang natively support JS (ES5), so if your language compiles to clean JS, you are ready
  * Mulang natively supports Haskell and Prolog, so... the same as before
  * Mulang natively supports Json, so if you can generate a JSON AST for your language, you are done. And every language can :wink:

And we are extending Mulang everyday, so more expectations - that is, the queries you can do against Mulang - are being frequently added. And more languages are going to be supported natively. The pipelines contains the following:

  * C
  * Java
  * Python

## Installing it

Mulang is just a Haskell library. You can install it though cabal.

But if you are not the Haskell inclined gal or guy - ok, I will try to forgive you - this code comes with a command line too. So you don't even have to typecheck!

Sample CLI usage:

```
$ mulang '{"expectations":[{"subject":["x"],"transitive":false,"negated":false,"object":{"tag":"Anyone","contents":[]},"verb":"uses"}],"code":{"content":"x = 1","language":"Haskell"}} '
{"results":[{"result":false,"expectation":{"subject":["x"],"transitive":false,"negated":false,"object":{"tag":"Anyone","contents":[]},"verb":"uses"}}],"smells":[]}
```

## Building mulang from source
To generate `mulang` executable, you have to build the project using [cabal](https://www.haskell.org/cabal/).

### Before start

**The first time**, you need to install [alex](https://www.haskell.org/alex/) and [happy](https://www.haskell.org/happy/), two executables that Mulang uses:
```
sudo apt-get install alex
sudo apt-get install happy
```
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
