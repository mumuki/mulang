Mulang
======
> The Universal, Multi Language, Multi Paradigm code analyzer

[![Build Status](https://travis-ci.org/mumuki/mulang.svg)](https://travis-ci.org/mumuki/mulang)

## Getting Started

Better than explaining what is Mulang, let's see what can do it for you.

Let's starts simple - we have the following JS expression:

```javascript
var pepita = {lugar: bsAs1, peso: 20};
var bsAs1 = bsAs
```

We want to recognize some code patters of it, so we will first load the expression into Mulang:

```
$ ghci
> :m Language.Mulang.All
> let e = js "var pepita = {lugar: bsAs1, peso: 20}; var bsAs1 = bsAs"
```

Now magic begins. We want to know if the code expression uses a certain binding - that is a variable, function, or anything that has a name:

```haskell
> hasUsage "bsAs" e
True
> hasUsage "rosario" e
False
```

That was easy, but just in case you are wondering, no, Mulang didn't do a `string.contains` or so,mething like that :stuck_out_togue: :

```haskell
> hasUsage "bs" e
False
```

So let's ask something more interesting - does `bsAs1` use the binding `bsAs`?

```haskell
> scoped (hasUsage "bsAs") "bsAs1"  e
True
```

And does it use `rosario`?

```haskell
> scoped (hasUsage "rosario") "bsAs1"  e
False
```

What about the object `pepita`? Does it use `bsAs1` or `rosario`?

```haskell
> scoped (hasUsage "bsAs1") "pepita"  e
True
> scoped (hasUsage "rosario") "pepita"  e
False
```

Does `pepita` use `bsAs`?

```haskell
> scoped (hasUsage "bsAs") "pepita"  e
False
```

Oh, wait there! We now, it is true that it does not use **exactly** that variable, but, come on, `bsAs1` does use `bsAs`! Wouldn't it be sweet to be transitive?

You ask for it, you get it:

```haskell
> transitive (hasUsage "bsAs") "pepita"  e
True
```

I know what you are thinking:  now you wan't to be stricter, you want to know if `pepita.lugar` uses bsAs1 - ignoring that `peso` attribute. Piece of cake:

```haskell
> scopedList (hasUsage "bsAs1") ["pepita", "lugar"]  e
True
> scopedList (hasUsage "bsAs1") ["pepita", "peso"]  e
False
```

Nice, we know. But not very awesome, it only can tell you if you are using a _binding_, right? Eeer. Good news, it can tell you much much much more things:


* `hasMethod`,
* `hasAttribute`
* `hasFunction`
* `hasArity` - that is, does the given method, function, procedure, etc have the given amount of parameters?
* `hasIf`
* `hasWhile`
* `hasLambda`
* `hasDirectRecursion`
* `hasGuards`
* `hasComposition`
* `hasComprehensions`
* `hasTypeSignature`
* `hasTypeAlias`
* `hasAnonymousVariable`
* `hasRedundantIf`
* `hasRedundantGuards`
* `hasRedundantParameter`
* `hasRedundantLambda`
* `doesTypeTest`
* `doesNullTest`
* `returnsNull`


And the really awesome is here: it works for every - yes, we said it - language on the world, because:

  * Mulang natively support JS (ES5), so if your language compiles to clean JS, you are ready
  * Mulang natively supports Haskell, so... the same as before
  * Mulang natively supports Json, so if you can generate a JSON AST for your language, you are done. And every language can :wink:


And we are extending Mulang everyday, so more expectations - that is, the queries you can do against Mulang - are being frequently added. And more languages are going to be supported natively. The pipelines contains the following:

  * C
  * Java
  * Prolog
  * Python

## Installing it

Mulang is just a Haskell library. You can install it though cabal.

But if you are not the Haskell inclined gal or guy - ok, I will try to forgive you - there is a command line too, (mulang-cli)[https://github.com/mumuki/mulang-cli]. So you don't even have to typecheck!