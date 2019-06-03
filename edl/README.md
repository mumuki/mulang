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

...which is just syntactic sugger - the `something` keyword is just a dummy token you can add to make the expectation syntax more human-readable.

Although you can declared unnamed expectations, it is usually more conventient to add an intention-revealing name:

```
expectation "you should call something":
  calls something;
```

The inspection names match exactly with th standard expectation format, but it is more releax, you can it write it in three diferent formats:

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

Like with standard expectations, you can use predicates with the inspections. The simplest of them is the _named_ predicate, that matches a name exactly againts the given identifier:

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

Such operations don't have an eqivalence on the standard expectations, and are unique to EDL.

## Scopes

Also, custom expectations mmy be scoped using the `within` and `through` operators,
which allow to inspect only a given portion of the code,

```
expectation "`HouseBuilder` must raise something":
  %% this will work if within the lexical scope of
  %% HouseBuilder a raise statement is used
  within `HouseBuilder` raises something"

expectation "`HouseBuilder` must raise something":
  %% this will work if within the lexical scope of
  %% HouseBuilder a raise statement is used, or if any code outside
  %% the lexical scope of HouseBuilder raises an excepcion
  through `HouseBuilder` raises something"

expectation "`HouseBuilder.builder` must create a new `House`":
  within `HouseBuilder.builder` instantiates `House`"

expectation "In the contexto of :
  through `Tree` calls something distinct of `foo`;

```







## Operators Reference

* `not`, `and`, `or`: they answer a scoped inspections that operates scoped inspections logically
* `count`: it answers a counter that counts a given inspection
* `+`: it answers a counter that sums two different counter
* `=`, `>=`, `<=`: it answers an unscoped inspection that compares a counter
* `within`, `through`: it scopes an unscoped inspection



### Using literal matchers

```
expectation "`exit` must be called with value 0":
  calls `exit` with 0;

expectation "`tell` must be called with value 10 and true":
  calls `tell` with (10, true);

expectation "`play` must be called with this":
  calls `play` with self;
```

### Using arithmethic and boolean logic matchers

```
expectation "the method `getTotalAmount` must return an arithmetic expresion":
  within `getTotalAmmount` returns with math;

expectation "a method that performs boolean operations must be declared":
  declares method with logic;
```

### Using nested matchers

```
expectation "must declare a procedure that uses ifs":
  declares procedure that (uses if)

expectation "must declare a procedure that uses ifs but not while":
  declares procedure that (uses if && ! uses while)
```

## Using counters

```
expectation "must perform at least three calls":
  count(calls) >= 3;

expectation "must declare three instances of `Tax`":
  count(inherits `Tax`) = 3;

expectation "must declare three subclases of `Tax` and two subclases of `Product`":
  count(inherits `Tax`) = 3 && count(inherits `Product`) = 2;

expectation "must declare no more than 4 methods in class `Queue`":
  within `Queue` count(declares method) <= 4;
```
