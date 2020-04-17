# Mulang

> A universal, multi-language, multi-paradigm code analyzer

Mulang is a tool for analysing source code, which is built on top of five main components:

  1. an [Abstract Semantic Tree](./astspec/), an intermediate language which allows to express the semantic - as opposed to syntatic - structure of a multi-paradigm program;
  2. a set of more than 90 [inspections](./inspections) for querying code querying code either explicitly - _expectations_ - or implicitlt - _smells_.
  3. an [Expectations Definition Language (EDL)](./edlspec), a language for defining custom expectations
  4. a [command line tool](./clispec/) for analysing both source code in many languages and Mulang's AST. This tool is distributed as both a `linux-amd64` binary and a JavaScript package. See [downloads section](https://github.com/mumuki/mulang/releases).
  5. higher level interfaces in [ruby](https://rubygems.org/gems/mulang) and [javascript](https://www.npmjs.com/package/mulang) that are easier to use and provides some additional capabilities like expectations parsing and automatic internationalized humanization.

## Supported languages

Mulang can work with many different programming languages. it natively supports:

  * C
  * Haskell
  * Java
  * JavaScript (ES6)
  * Python (3 and 4)
  * Prolog

In addition, through external tools, it offers support for the following languages:

  * Ruby, using [mulang-ruby](https://github.com/mumuki/mulang-ruby)
  * PHP, using [mulang-php](https://github.com/mumuki/mulang-php)
  * Gobstones, using [gs-weblang-cli](https://github.com/gobstones/gs-weblang-cli)
  * Sratch, using [mulang-scratch](https://github.com/mumuki/mulang-scratch)

If you want to use it with a different language, you will have to:

* either add explicit support in this repo - we love Pull Requests :heart: -, or
* translate your language into one of the natively supported ones, or
* translate your language to the Mulang JSON AST

##  Quick start

> The following section uses the [ruby interface](https://rubygems.org/gems/mulang) for demonstration purposes.
> However, you also can run all the examples either using the [comand line tool](./clispec/), or the [javascript](https://www.npmjs.com/package/mulang) interface.
> The javascript version is also available as [a ruby gem](https://rubygems.org/gems/mulangjs), suitable for Rails integration

Better than explaining what Mulang is, let's see what it can do for you.

### Inspections

Let's suppose we have the following JS code...

```javascript
var aPlace = buenosAires;
var aBird = {position: aPlace, weight: 20};
```

...and we want to recognize some code patterns on it. We will first load the expression into Mulang:

```ruby
> require "mulang"
> code = Mulang::Code.native "JavaScript", "var aPlace = buenosAires; var aBird = {position: aPlace, weight: 20};"
```

Now we want to know if the code expression _uses_ - that is, contains any reference to - a given _identifier_. Such identifier could be a variable, function, or anything that has a name:

```ruby
> code.expect 'Uses:buenosAires'
=> true # because of the reference in `...aPlace = buenosAires...`
> code.expect 'Uses:rosario'
=> false # no reference to the identifier `rosario` is found on the code
```

`Uses:buenosAires` is our first _inspection_: a function that takes a Mulang AST and answers a boolean question about it. That _seems_ easy, but just in case you are wondering: no, Mulang doesn't perform a `string.contains` or something like that :stuck_out_tongue: :

```ruby
> code.expect 'Uses:BuenosAires'
=> false # no reference to the identifier `buenos` is found on the code
```

### Contexts

So let's ask something more interesting - does `aPlace` _use_ the identifier `buenosAires`?

```ruby
> code.expect 'aPlace', 'Uses:buenosAires'
=> true # again, because of the the reference in `...aPlace = buenosAires...`
> code.expect 'aPlace', 'Uses:aBird'
=> false # because `...aPlace = buenosAires...` does not reference `aBird`...
```

Here we have contextualized the inspection, so it runs only within the contexts of the given binding.

Let's tray again: does `"aPlace"` _use_ `rosario`? And what about the object `aBird`? Does it use `aPlace` or `rosario`?

```ruby
> code.expect "aPlace", "Uses:rosario"
=> false
> code.expect "aBird", "Uses:aPlace"
=> true
> code.expect "aBird",  "Uses:buenosAires"
=> true
```

Oh, wait! Is this a bug? Nope. Expectations are _transitive_ by default: `aBird` uses `aPlace`, and `aPlace` uses `buenosAires` in turn, which means that `aBird` actually _uses_ `buenosAires`. If you don't want that behaviour you can turn it off:

```ruby
> code.expect "Intransitive:aBird",  "Uses:buenosAires"
=> false
```

_Contexts_ can be nested, too: for example, if you want to know whether `aBird.position` _uses_ `aPlace` - ignoring that `weight` attribute:

```ruby
> code.expect "aBird.position", "Uses:aPlace"
=> true
> code.expect "aBird.weight", "Uses:aPlace"
=> false
```

> Inspections do not only allow you to consult usages. They can tell you much more things. See the [supported inspections list]('./inspections/).

### Predicates

Many inspections support an _identifier predicate_, that is, a matcher for the identifier.

For example, does the former piece of code declare any attribute?

```ruby
> code.expect 'DeclaresAttribute:*'
=> true
> code.expect 'DeclaresAttribute' # shorter version
=> true
```

Does it declare an attribute like `eight`?


```ruby
> code.expect "DeclaresAttribute:~eight"
=> true
```
> Notice: `like` in Mulang means that it contains that case-insensitive substring

Finally, does `aBird` declares an attribute that is not named `weight`?

```ruby
> code.expect "DeclaresAttribute:^weight"
=> true # because it also declares position
```

### Matchers

Finally, you can provide a _matcher_ to many of the available expectations, that allows to match specific parts of code with some patterns.

```ruby
> code.expect 'DeclaresAttribute:WithNumber:20'
=> true  # because weight is initialized with that value
> code.expect 'DeclaresAttribute:WithNumber:21'
=> false
> code.expect 'DeclaresAttribute:position:WithNonliteral'
=> true # because position is initialized with a non-literal value
> code.expect 'DeclaresAttribute:WithLiteral'
=> true # because weight is initialized with a literal value
> code.expect 'DeclaresAttribute:WithNil'
=> false # because no attribute is initialied with null
```

The complete list of supported matchers is the following:

  * `WithFalse`
  * `WithLiteral`
  * `WithLogic`
  * `WithMath`
  * `WithNil`
  * `WithNonliteral`
  * `WithTrue`
  * `WithChar:'value'`
  * `WithSymbol:value`
  * `WithNumber:value`
  * `WithString:"value"`


# Contributors

 * Franco Bulgarelli @flbulgarelli @ [Mumuki](@mumuki)
 * Julian Berbel Alt @julian-berbel @ [Mumuki](@mumuki)
 * Federico Lochbaum @FedeLochbaum @ [UNQ](http://www.unq.edu.ar/)
 * Lucas Traverso @ludat @ [10Pines](@10pines)
