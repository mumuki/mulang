# Expectations Definition Language Reference

EDL allows to express more complex expectations by combining existing inspection.

## Basic syntax


```
expectation [<name>]:
  <expectation body>;
```

For example, this is the simplest expectation you can declare:

```
expectation:
  calls;
```

This expectation simple checks that something is called - like a function, a procedure - and it behaves exactly as the `* Calls:*` expectation.

If you want to make it more explicit that you are expecting to call _something_, you can also write it this way...

```
expectation:
  calls something;
```

...which is just syntactic sugar - the `something` keyword is just a dummy token you can add to make the expectation syntax more human-readable.

Although you can declare unnamed expectations, it is usually more convenient to add an intention-revealing name:

```
expectation "you should call something":
  calls something;
```

The inspection names match exactly with the standard expectation format, but it is more relaxed, you can it write it in three diferent formats:

 * `UpperCamelCase`, which is the standard syntax, e.g. `UsesIf`
 * `lowerCamelCase`, e.g. `usesIf`
 * `case insensitive words`, e.g. `uses if`, `Uses If` and `uses IF`

However, using lower case words - `uses if` - is the preferred case. This means that all the following declarations are equivalent:

```

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

```
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

```
expectation "`System` must be used":
  %% equivalent to the standard expectation * Uses:=System
  uses `System`;

expectation "`exit` must be called":
  %% equivalent to the standard expectation * Calls:=exit
  calls `exit`;
```

However, all the predicates are available to EDL:

```
%% matches things like `amount`, `totalAmount` or `amountOfHouses`
expectation "assignment a variable similar to `amount`":
  %% equivalent to * Assigns:~amount
  assigns something like `amount`;

expectation "must declare something aside of `Pet`":
  %% equivalent to * Declares:^Pet
  declares something distinct of `Pet`;

expectation "must declare a class aside of `Dog`":
  %% equivalent to * DeclaresClass:^Dog
  declares class distinct of `Dog`;

expectation "must declare `feed` or `bark`":
  %% equivalent to * DeclaresMethod:[feed|bark]
  declares method in (`feed`, `bark`);

expectation "must call `feed` or `bark`":
  %% equivalent to * Calls:[feed|bark]
  calls something in (`feed`, `bark`);
```

## Boolean operators on unscoped inspections

EDL allows to use logical operations with - _unscoped_, more on that later - inspections:

```
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

```
expectation "`HouseBuilder` must raise something":
  %% this will work if within the lexical scope of
  %% HouseBuilder a raise statement is used
  %% equivalent to Intransitive:HouseBuilder Raises
  within `HouseBuilder` raises something";

expectation "`HouseBuilder` must raise something":
  %% this will work if within the lexical scope of
  %% HouseBuilder a raise statement is used, or if any code outside
  %% the lexical scope of HouseBuilder raises an exception
  %% equivalent to HouseBuilder Raises
  through `HouseBuilder` raises something";

expectation "`HouseBuilder.builder` must create a new `House`":
  %% equivalent to Instransitive:HouseBuilder.builder Instantiates:House
  within `HouseBuilder.builder` instantiates `House`";

expectation "In the context of `Tree`, something other than `foo` is called":
  %% equivalent to Tree Calls:^foo
  through `Tree` calls something distinct of `foo`;
```

## Matchers

Some inspections support an optional matcher argument, that allow you to provide
additional conditions using the `with` keyword:

```
expectation "`exit` must be called with value 0":
  %% equivalent to * Calls:exit:WithNumber:0
  calls `exit` with 0;

expectation "`tell` must be called with value 10 and true":
  %% equivalent to * Calls:tell:WithNumber:10:WithTrue
  calls `tell` with (10, true);

expectation "`play` must be called with this":
  %% equivalent to * Calls:play:WithSelf
  calls `play` with self;
```

Most of the matchers are designed to perform literal queries, but some of them allow more complex matching:

```
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

```
expectation "must declare a class that uses an if":
  declares class with something that (uses if);

expectation "must declare a class that uses an if":
  %% shorter, equivalent version of previous example
  declares class that (uses if);

expectation "`VirtualPet.HappyState` must declare a getter that returns a literal value":
  within `VirtualPet.HappyState`
  declares method like `get`
  that (returns with literal);
```

`that`-queries can be nested, thus allowing you to write quite abstract expectations:

```
expectation "package `tamagochi` must declare a class with a method that returns an arithmethic expression":

  within `tamagochi`
  declares class
  that (
    declares method
    that (
      returns with math));
```

Previous logical operators may also be used with `that`-queries:


```
expectation "must declare a procedure that uses ifs":
  declares procedure
  that (uses if);

expectation "must declare a procedure that uses ifs but not while":
  declares procedure
  that (uses if && ! uses while);
```


### Supported inspections

This is the complete list of inspections that support matchers:

* `assigns`: accepts a matcher that matches the assigned value
* `calls`: accepts a matcher that matches the passed arguments
* `declares class`: accepts a matcher that matches any of the body expressions
* `declares function`: accepts a matcher that matches any of the body expressions
* `declares interface`: accepts a matcher that matches any of the body expressions
* `declares method`: accepts a matcher that matches any of the body expressions
* `declares object`: accepts a matcher that matches any of the body expressions
* `declares procedure`: accepts a matcher that matches any of the body expressions
* `declares variable`: accepts a matcher that matches the initial value
* `returns`: accepts a matcher that matches the returned value

### Supported matchers

* `(<matcher1>, <matcher2>.., <matcherN>)`: matches a tuple of arguments
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

EDL allows you to define expectations by counting and comparing matches of another inspection, by using the `count`, `=`, `>=` and `<=` operators:

```
expectation "must perform at least three calls":
  count(calls) >= 3;

expectation "must declare three instances of `Tax`":
  count(inherits `Tax`) = 3;

expectation "must declare three subclasses of `Tax` and two subclasses of `Product`":
  count(inherits `Tax`) = 3 && count(inherits `Product`) = 2;

expectation "must declare no more than 4 methods in class `Queue`":
  within `Queue` count(declares method) <= 4;
```

Finally, counters may be added, using the `+` opertor:

```
expectation "The `Bar` must declare 4 or more methods related to beer or whisky":
  within `Bar`
  count (declares method like `whisky`) + count (declares method like `beer`) >= 4;
```

## Supported counters

Not all inspections can be counted. Currently, only the following are supported:

* `uses if`
* `uses for`
* `declares attribute`
* `declares class`
* `declares function`
* `declares interface`
* `declares method`
* `declares object`
* `declares procedure`
* `declares variable`
