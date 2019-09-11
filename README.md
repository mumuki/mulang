[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

# Mulang

> A universal, multi-language, multi-paradigm code analyzer

Mulang is a tool for analysing source code, which is built on top of five main components:

  1. an [Abstract Semantic Tree](https://mumuki.github.io/mulang/astspec/), an intermediate language which allows to express the semantic - as opposed to syntatic - structure of a multi-paradigm program;
  2. a set of more than 90 [inspections](https://mumuki.github.io/mulang/inspections) for querying code querying code either explicitly - _expectations_ - or implicitlt - _smells_.
  3. an [Expectations Definition Language (EDL)](https://mumuki.github.io/mulang/edlspec), a language for defining custom expectations
  4. a [command line tool](https://mumuki.github.io/mulang/clispec/) for analysing both source code in many languages and Mulang's AST. This tool is distributed as both a `linux-amd64` binary and a JavaScript package. See [downloads section](https://github.com/mumuki/mulang/releases).
  5. higher level interfaces in [ruby](https://rubygems.org/gems/mulang) and [javascript](https://www.npmjs.com/package/mulang) that are easier to use and provides some additional capabilities like expectations parsing and automatic internationalized humanization.




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


# Customs Expectations and the Expectations Definition Language

Mulang accepts custom-made expectations expressed in an Expectations Definition Language - EDL - which allows to build complex
expectations by combining existing inspections.


# Contributors

 * Franco Bulgarelli @flbulgarelli [Mumuki](@mumuki)
 * Julian Berbel Alt @julian-berbel @ [Mumuki](@mumuki)
 * Federico Lochbaum @FedeLochbaum @ [UNQ](http://www.unq.edu.ar/)
 * Lucas Traverso @ludat @ [10Pines](@10pines)
