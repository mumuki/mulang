# Expectations Definition Language Reference

EDL allows to express more complex expectations by combining existing inspection.

## Basic syntax


```edl
expectation [<name>]:
  <expectation body>;
```

For example, this is the simplest expectation you can declare:

```edl
expectation:
  calls;
```

This expectation performs a simple _query_ that checks that something is called - like a function, a procedure - and it behaves exactly as the `* Calls:*` expectation.

If you want to make it more explicit that you are expecting to call _something_, you can also write it this way...

```edl
expectation:
  calls something;
```

...which is just syntactic sugar - the `something` keyword is just a dummy token you can add to make the expectation syntax more human-readable.

Although you can declare unnamed expectations, it is usually more convenient to add an intention-revealing name:

```edl
expectation "you should call something":
  calls something;
```

The inspection names match exactly with the standard expectation format, but it is more relaxed, you can it write it in three diferent formats:

 * `UpperCamelCase`, which is the standard syntax, e.g. `UsesIf`
 * `lowerCamelCase`, e.g. `usesIf`
 * `case insensitive words`, e.g. `uses if`, `Uses If` and `uses IF`

However, using lower case words - `uses if` - is the preferred case. This means that all the following declarations are equivalent:

```edl
% ok
expectation:
  UsesIf;

% ok
expectation:
  usesIf;

% better
expectation:
  uses if;
```

More examples:

```edl
expectation "must declare something":
  declares;

expectation "must declare something":
  %% exactly the same as previous example
  declares something;

expectation "assignment operator must be used":
  assigns;

expectation "a class must be declared":
  declares class;

expectation "a function must be declared":
  declares function;

```

## Predicates

Like with standard expectations, you can use predicates with the inspections. The simplest of them is the _named_ predicate, that matches a name exactly against the given identifier:

```edl
expectation "`System` must be used":
  %% equivalent to the standard expectation * Uses:=System
  uses `System`;

expectation "`exit` must be called":
  %% equivalent to the standard expectation * Calls:=exit
  calls `exit`;
```

However, all the predicates are available to EDL:

```edl
%% matches things like `amount`, `totalAmount` or `amountOfHouses`
expectation "assignment a variable similar to `amount`":
  %% equivalent to * Assigns:~amount
  assigns something like `amount`;

expectation "must declare a class aside of `Dog`":
  %% equivalent to * DeclaresClass:^Dog
  declares class except `Dog`;
```

Also, there EDL exposes more predicates:

```edl
expectation "declares methods apart from getters":
  declares method unlike `get`;

expectation "must declare `feed` or `bark`":
  declares method in (`feed`, `bark`);

expectation "must call `feed` or `bark`":
  calls something in (`feed`, `bark`);

expectation "must call something similar to `feed` or `bark`":
  calls something like in (`feed`, `bark`);

expectation "must call something apart from `feed` or `bark`":
  calls something except in (`feed`, `bark`);

expectation "must declare something apart from classes like `Base` or `Abstract`":
  declares class unlike in (`Base`, `Abstract`);
```

## Boolean operators on unscoped queries

EDL allows to use logical operations with - _unscoped_, more on that later - queries:

```edl
%% negation
expectation "must not call anything":
  ! calls something;

%% logical-or
expectation "must declare a class, enum or interface named `Pet`":
  declares enumeration `Pet` || declares class `Pet` || declares interface `Pet`;

%% logical-and
expectation "must declare `Owner` and `Pet`":
  %% however in most cases, it is better to declare two different, separate
  %% expectations
  declares `Owner` && declares `Pet`;
```

Such operations don't have an equivalence on the standard expectations, and are unique to EDL.

## Scopes

Also, custom expectations mmy be scoped using `within` and `through`,
which allow inspecting only a given portion of the code: `within` performs intransitive cuts while
`through` performs transitive cuts.

```edl
expectation "`HouseBuilder` must raise something":
  %% this will work if within the lexical scope of
  %% HouseBuilder a raise statement is used
  %% equivalent to Intransitive:HouseBuilder Raises
  within `HouseBuilder` raises something;

expectation "`HouseBuilder` must raise something":
  %% this will work if within the lexical scope of
  %% HouseBuilder a raise statement is used, or if any code outside
  %% the lexical scope of HouseBuilder raises an exception
  %% equivalent to HouseBuilder Raises
  through `HouseBuilder` raises something;

expectation "`HouseBuilder.builder` must create a new `House`":
  %% equivalent to Instransitive:HouseBuilder.builder Instantiates:House
  within `HouseBuilder.builder` instantiates `House`;

expectation "In the context of `Tree`, something other than `foo` is called":
  %% equivalent to Tree Calls:^foo
  through `Tree` calls something except `foo`;
```

Queries that use a scope operator are called _scoped queries_. Conversely _unscoped queries_ are those that don't use one.

## Matchers

Some inspections support an optional matcher argument, that allow you to provide
additional conditions using the `with` keyword:

```edl
expectation "`exit` must be called with value 0":
  %% equivalent to * Calls:exit:WithNumber:0
  calls `exit` with 0;

expectation "`tell` must be called with value 10 and true":
  %% equivalent to * Calls:tell:WithNumber:10:WithTrue
  calls `tell` with (10, true);

expectation "`play` must be called with this":
  %% equivalent to * Calls:play:WithSelf
  calls `play` with self;

expectation "uses a repeat with a non-literal amount of iterations":
  %% matches repeat blocks where the repeat count expression is a non-literal
  %% and the loop body is anything
  uses repeat with (nonliteral, anything);

expectation "uses a repeat with a non-literal amount of iterations":
  %% shorter version of previous example
  uses repeat with nonliteral;
```

Most of the matchers are designed to perform literal queries, but some of them allow more complex matching:

```edl
expectation "the method `getTotalAmount` must return an arithmetic expresion":
  %% equivalent to Intransitive:getTotalAmmount Returns:WithMath
  within `getTotalAmmount` returns with math;

expectation "a method that performs boolean operations must be declared":
  %% equivalent to * DeclaresMethod:WithLogic
  declares method with logic;

expectation "`getAge` must not return a hardcoded value:
  %% equivalent to Intransitive:getAge Returns:WithNonliteral
  within `getAge` returns with nonliteral;
```

As you can see in previous examples, many of the simplest matchers can also be used in the standard expectation syntax. However, EDL also supports the `that` matcher,
that allows you to build complex, nested queries:

```edl
expectation "must declare a class that uses an if":
  declares class with something that (uses if);

expectation "must declare a class that uses an if":
  %% shorter, equivalent version of previous example
  declares class that (uses if);

expectation "`VirtualPet.HappyState` must declare a getter that returns a literal value":
  within `VirtualPet.HappyState`
  declares method like `get`
  that (returns with literal);

expectation "uses a c-style for-loop that declares variable `x`":
  uses for loop with (
    something that (declares variable `x`),
    anything,
    anything,
    anything);

expectation "uses a c-style for-loop that declares variable `x`":
  %% even shorter version of previous example
  uses for loop that (declares variable `x`);
```

`that`-queries can be nested, thus allowing you to write quite abstract expectations:

```edl
expectation "package `tamagochi` must declare a class with a method that returns an arithmethic expression":
  within `tamagochi`
  declares class
  that (
    declares method
    that (
      returns with math));

expectation "repeats `next()` 3 times":
  uses repeat with (3, something that (calls `next`));
```

Previous logical operators may also be used with `that`-queries:


```edl
expectation "must declare a procedure that uses ifs":
  declares procedure
  that (uses if);

expectation "must declare a procedure that uses ifs but not while":
  declares procedure
  that (uses if && ! uses while);

expectation "does not nest a while within an if":
  ! uses if with (
    anything,
    something that uses while,
    anything)
  && ! uses if with (
    anything,
    anything,
    something that (uses while));

expectation "uses an if that does not nest a while inside it":
  uses if with (
    anything,
    something that (!uses while),
    something that (!uses while));
```


### Supported inspections

This is the complete list of inspections that support matchers:

* `assigns`: accepts a matcher that matches the assigned value
* `calls`: accepts a matcher that matches the passed arguments
* `declares attribute`: accepts a matcher that matches the initial value
* `declares class`: accepts a matcher that matches any of the body expressions
* `declares entry point`: accepts a matchers that matches the entry point body
* `declares function`: accepts a matcher that matches any of the body expressions
* `declares interface`: accepts a matcher that matches any of the body expressions
* `declares method`: accepts a matcher that matches any of the body expressions
* `declares object`: accepts a matcher that matches any of the body expressions
* `declares procedure`: accepts a matcher that matches any of the body expressions
* `declares variable`: accepts a matcher that matches the initial value
* `returns`: accepts a matcher that matches the returned value
* `uses for each`: accepts a matcher that matches the generator, and the loop body
* `uses for loop`: accepts a matcher that matches the initializer expression, the condition expression, the increment expression and the loop body
* `uses if`: accepts a matcher that matches the condition, the `then` expression and the `else` expression
* `uses lambda`: accepts a matcher that matches the lambda body
* `uses print`: accepts a matcher that matchers the printed value
* `uses repeat`: accepts a matcher that matches the repeat expression and the loop body
* `uses while`: accepts a matcher that matches the condition expression and the loop body
* `uses yield`: accepts a matcher that matches the yielded value

### Supported matchers

* `(<matcher1>, <matcher2>.., <matcherN>)`: matches a tuple of expressions, like a callable's arguments or a control structure parts. If less elements than required are passed, the list is padded with `anything` matchers.
* `<character>`: matches a single character
* `<number>`: matches a number literal
* `<string>`: matches a single string
* `<symbol>`: matches a symbol literal
* `anything`: matches anything
* `false` and `true`: matches the `true` and `false` literals
* `literal`: matches any literal
* `logic`: matches a boolean expression
* `math`: matches an arithmetic expresion
* `nil`: matches the nil/null literal
* `nonliteral`: matches a non-literal expression
* `self`: matches the self/this literal
* `that (<other query>)`: matches an expression that makes the given query true


## Counters

EDL allows you to define expectations by counting and comparing matches of another query, by using the `count`, `=`, `>=` and `<=` operators:

```edl
expectation "must perform at least three calls":
  count(calls) >= 3;

expectation "must declare three instances of `Tax`":
  count(inherits `Tax`) = 3;

expectation "must declare three subclasses of `Tax` and two subclasses of `Product`":
  count(inherits `Tax`) = 3 && count(inherits `Product`) = 2;

expectation "must declare no more than 4 methods in class `Queue`":
  within `Queue` count(declares method) <= 4;

expectation "`max` must have 2 returns":
  within `max` count(returns) = 2;
```

Finally, counters may be added, using the `+` opertor:

```edl
expectation "The `Bar` must declare 4 or more methods related to beer or whisky":
  within `Bar`
  count (declares method like `whisky`) + count (declares method like `beer`) >= 4;
```

### Supported counters

Not all inspections can be counted. Currently, only the following are supported:

* `calls`
* `declares attribute`
* `declares class`
* `declares function`
* `declares interface`
* `declares method`
* `declares object`
* `declares procedure`
* `declares variable`
* `returns`
* `uses for each`
* `uses for loop`
* `uses for`
* `uses if`
* `uses lambda`
* `uses print`
* `uses repeat`
* `uses switch`
* `uses try`
* `uses while`
* `uses yield`

## Boolean operators on scoped queries

Finally, EDL allows to use logical operations with scoped queries:

```edl
%% negation
expectation "must not assign anything in `main`":
  not (within 'main' assigns);

%% logical-or
expectation "pacakge `vet` must declare a class, enum or interface named `Pet`":
  (within `vet` declares enumeration `Pet`)
  or (within `vet` declares class `Pet`)
  or (within `vet` declares interface `Pet`);

%% alternate definition, which is equivalent provided package `vet` exists
expectation "pacakge `vet` must declare a class, enum or interface named `Pet`":
  within `vet` declares enumeration `Pet` || declares class `Pet` || declares interface `Pet`;

%% logical-and
expectation "`Pet` must declare `eat` and `Owner` must send it":
  %% however in most cases, it is better to declare two different, separate
  %% expectations
  (within `Pet` declares `eat`) and (within `Owner` sends `eat`);
```

# ⚠️ Caveats

Mulang by default will apply autocorrection rules that transform language-dependant
inspections - like `Uses:length` in JavaScript - into proper operator inspections like `UsesSize`. Those transformation
**will not be applied when using EDL**, so be careful when using languages primitives.