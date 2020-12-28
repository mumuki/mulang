# Mulang AST spec

In this section, we will get into the technical details of the Mulang AST. It is built around 5 core elements:

* [Expressions](#expressions)
* [Patterns](#patterns)
* [Types](#types)
* Equations
* Generators

All the AST elements fall within any of these 5 categories.

## Expressions

Expressions are the most important element kind, since contain most of the information of a Mulang program and are always the root element of it. In fact, this implementation does not contain an `AST` or `Program` datatype - it is instead types as `Expression`.

Expression in Mulang model what you will normally spec in a language as a expression, that is something that holds a value and a type. For example, `4 + 5` and `[2, 3].toString()` are typical expresion.

However, Mulang extends this concept to most kind of elements in a program, regadless they are have an actual value in the original language. For example, class declarations and while statements are modeled as expression, although in many languages they aren't.

As a rule of thumb if something is or can be represented as an statement, declararion or expression, the it is modeled as `Expression` in Mulang AST.

### `Record`

> A `Record` represents a record, data or struct declaration,
> as found in most procedural and functional languages, like the C-like `struct` declaration

#### Syntax

```haskell
(Record Identifier)
```

#### C Example

```c
struct point {
  int x;
  int y;
}
```

```haskell
(Record "point")
```

#### Caveats

Currently, the `Record` expression does not hold information about the record contents.


### `TypeAlias`, `TypeSignature` and `TypeCast`

Mulang AST support for type analysis is quite limited, and it is mostly focused on expressions and declarations analysis. However, for sake of completeness and in order to provide some limited type-information in Mulang AST, `TypeAlias`, `TypeSignature` and `TypeCast` expressions are provided.

See [types section](#types) for more details.

### `EntryPoint`

> Entry point with its name and body. It typically correspond to C-like `main` procedures, or `program` declarations.

#### Syntax

```haskell
(EntryPoint Identifier Expression)
```

#### Java Example

```java
public static main(String[] args) {}
```

```haskell
(EntryPoint "main" MuNil)
```

### `Function`

> Functional / Imperative programming function declaration.
> It is is composed by an identifier and one or more equations

#### Syntax

```haskell
(Function Identifier [Equation])
```

#### C Example

```c
int foo (int bar) {
  return bar;
}
```

```haskell
Sequence [
  TypeSignature "foo" (ParameterizedType ["int"] "int" []),
  Function "foo" [
    Equation [VariablePattern "bar"]
      (UnguardedBody (Return (Reference "bar")))]]
```

#### Python Example

```python
def foo():
  return 1
```

```haskell
(Function "foo" [
  (Equation []
    (UnguardedBody (Return (MuNumber 1.0))))])
```

```python
def foo(bar):
  return bar
```

```haskell
(Function "foo" [
  (Equation [VariablePattern "bar"]
    (UnguardedBody (Return (Reference "bar"))))])
```

### `Procedure`

> Imperative programming procedure declaration. It is composed by an identifier and one or more equations

#### Syntax

```haskell
(Procedure Identifier [Equation])
```

### `Method`

> Object oriented programming method declaration. It is composed by an identifier and one or more equations


#### Syntax

```haskell
(Method Identifier [Equation])
```

#### Ruby Example

```ruby
class Bird
  def sing!
    puts "singing in the dead of night"
  end
end
```

```haskell
(Class "Bird" Nothing
  (Method "sing!" [
    (Equation []
      (UnguardedBody (Print (MuString "singing in the dead of night"))))]))
```

#### Java Example

```java
public class Bird {
  public void sing() {
    System.out.println("singing in the dead of night");
  }
}
```

```haskell
(Class "Bird" Nothing
  (Method "sing" [
    (Equation []
      (UnguardedBody (Print (MuString "singing in the dead of night"))))]))
```


### `PrimitiveMethod`

> Declaration of custom primitive operators - also known as operator overriding. See [primitive operators](#primitive-operators)

#### Syntax

```haskell
(PrimitiveMethod Operator [Equation])
```

#### Ruby Example

```ruby
def ==(other)
end

def hash
end
```

```haskell
(Sequence [
  (PrimitiveMethod Equal
    (Equation [VariablePatten "other"]
      (UnguardedBody MuNil))),

  (PrimitiveMethod Hash
    (Equation []
      (UnguardedBody MuNil)))])
```

### `Variable`

> Generic variable declaration, composed by an identifier and an initializer

#### Syntax

```haskell
(Variable Identifier Expression)
```

#### C Example

```c
int a = 10;
```

```haskell
(Sequence [
  TypeSignature "a" (SimpleType "int" []),
  Variable "a" (MuNumber 10.0)])
```

#### JavaScript Example

```javascript
let x = 1;
```

```haskell
(Variable "x" (MuNumber 1))
```

### `Assignment`

#### Syntax

```haskell
(Assignment Identifier Expression)
```

#### C Example

```c
m = 3.4;
```

```haskell
(Assignment "m" (MuNumber 3.4))
```

#### Ruby Example

```ruby
m = 3.4
```

```haskell
(Assignment "m" (MuNumber 3.4))
```

### `Attribute`

> Object oriented programming attribute declaration, composed by an identifier and an initializer

#### Syntax

```haskell
(Attribute Identifier Expression)
```

#### Java Example

```java
public class Foo {
  private int bar = 4;
}
```

```haskell
(Class "Foo" Nothing
  (Sequence [
    (VariableSignature "bar" "int" []),
    (Attribute "bar" (MuNumber 4))]))
```

### `Object`

> Object oriented programming global, named object declaration, like Scala's `object`, composed by a name and a body.

#### Syntax

```haskell
(Object Identifier Expression)
```

#### Example

### `Class`

> Object oriented programming global, class declaration,
> composed by a name, an optional superclass and a body

#### Syntax

```haskell
(Class Identifier (Maybe Identifier) Expression)
```

#### Ruby Example

```ruby
class Bird < Animal
end
```

```haskell
(Class "Bird" (Just "Animal") MuNil)
```

#### Java Examples

```java
public class Bird extends Animal {}
```

```haskell
(Class "Bird" (Just "Animal") MuNil)
```

### `Enumeration`

> Imperative named enumeration of values

#### Syntax

```haskell
(Enumeration Identifier [Identifier])
```


#### Java Example

```java
public enum Fuzzy {
  YES, NO, MAYBE
}
```

```haskell
(Enumeration "Fuzzy" ["YES", "NO", "MAYBE"])
```

### `Interface`

> Object oriented programming global interface or contract declaration, composed by a name, superinterfaces and a body.

#### Syntax

```haskell
(Interface Identifier [Identifier] Expression)
```

#### Java Example

```java
public interface Foo extends Bar, Baz {
  void foo();
}
```

```haskell
(Interface
    "Foo"
    ["Bar", "Baz"]
    (TypeSignature "foo" [] "void"))
```

### `Rule`

> Logic programming declaration of rule fact, composed by the rule name, rule arguments, and rule body

#### Syntax

```haskell
(Rule Identifier [Pattern] [Expression])
```

#### Example

```prolog
baz(bar) :- foo(bar)
```

```haskell
(Rule "baz"
    [(LiteralPattern "bar")]
    [(Exist "foo"
        [(LiteralPattern "bar")])])
```

### `Fact`

> Logic programming declaration of a fact , composed by the fact name and fact arguments

#### Syntax

```haskell
(Fact Identifier [Pattern])
```

#### Example

```prolog
foo(bar).
```

```haskell
(Fact "foo" [(LiteralPattern "bar")])
```

### `Exist`

> Logic programming existential cuantification / consult

#### Syntax

```haskell
(Exist Identifier [Pattern])
```


#### Example

### `Not`

> Logic programming negation

#### Syntax

```haskell
(Not Expression)
```

### `Findall`

> Logic programming findall

#### Syntax

```haskell
(Findall Expression Expression Expression)
```

### `Forall`

> Logic programming universal cuantification

#### Syntax

```haskell
(Forall Expression Expression)
```

### `Reference`

> Generic variable

#### Syntax

```haskell
(Reference Identifier)
```

#### C Example

```javascript
int x = 4;
x
```

```haskell
(Sequence [
  TypeSignature "x" (SimpleType "int" []),
  Variable "x" (MuNumber 4.0),
  Reference "x"])
```

#### JavaScript Example

```javascript
const x = 4;
x
```

```haskell
(Sequence [
  (Variable "x" (MuNumber 4.0),
  (Reference "x"))])
```

### `Application`

> Generic, non-curried application of a function or procedure,
> composed by the applied element itself, and the application arguments

#### Syntax

```haskell
(Application Expression [Expression])
```

#### Example

### `Send`

> Object oriented programming message send, composed by the reciever,
> selector and arguments

#### Syntax

```haskell
(Send Expression Expression [Expression])
```

#### Ruby Example

```ruby
1 + 5
```

```haskell
(Send (MuNumber 1) (Reference "+") [MuNumber 5])
```

### `New`

> Object oriented instantiation, composed by the class reference and instantiation arguments

#### Syntax

```haskell
(New Identifier [Expression])
```
### `Implement`

> Object oriented instantiation, interface implementation

#### Syntax

```haskell
(Implement Identifier)
```

### `Include`

> Object oriented instantiation, mixin inclusion

#### Syntax

```haskell
(Include Identifier)
```

### `If`

#### Syntax

```haskell
(If Expression Expression Expression)
```
### `Lambda`

#### Syntax

```haskell
(Lambda [Pattern] Expression)
```

### `Return`

#### Syntax

```haskell
(Return Expression)
```

### `While`

> Imperative programming conditional repetition control structure, composed by a condition and a body

#### Syntax

```haskell
(While Expression Expression)
```

### `Repeat`

> Imperative programming fixed repetition control structure, composed by a repetition count expression, and a body

#### Syntax

```haskell
(Repeat Expression Expression)
```

### `Match`

#### Syntax

```haskell
(Match Expression [Equation])
```

### `Switch`

> Generic switch expression, composed by the value to switch on, a list of cases and the default

#### Syntax

```haskell
(Switch Expression [(Expression, Expression)] Expression)
```

### `Try`

> Generic try expression, composed by a body, a list of exception-handling patterns and statments, and a finally expression

#### Syntax

```haskell
(Try Expression [(Pattern, Expression)] Expression)
```

### `Raise`

> Generic raise expression, like a `throw` or `raise` statament, composed by the raised expression

#### Syntax

```haskell
(Raise Expression)
```

#### JavaScript Example

```javascript
throw 'abc';
```

```haskell
(Raise (MuString "abc"))
```

### `Print`

> Generic print expression

#### Syntax

```haskell
(Print Expression)
```

#### Ruby Example

```ruby
puts "Hello World"
```

```haskell
(Print (MuString "Hello World"))
```

### `For`

> `For`s generalices the concept of comprehensions an indexed repetition. With a `For` you can build:
>
> * `ForComprehension`, when the for expression is a yield. Scala's `for` comprehensions, Erlang's and Haskell's list comprehensions, and Haskell's `do-syntaxt` map to it.
> * `ForEach`, when the for expression is not a yield.  Java's `for:`, or some scenarios of scala's `for` map to it.

#### Syntax

```haskell
(For [Statment] Expression)
```

#### Haskell Example

```haskell
m = [ f x | x <- [1, 2, 3, 4] ]
```

```haskell
(Variable "m"
  (For
    [(Generator
      (VariablePattern "x")
      (MuList [(MuNumber 1), (MuNumber 2), (MuNumber 3), (MuNumber 4)]))]
    (Yield
      (Application (Reference "f") [(Reference "x")]))))
```

#### Java Example

```java
for (Integer i : ints) {
  System.out.println(i);
}
```

```haskell
(For
  [(Generator
      (VariablePattern "i")
      (Reference "ints"))]
  (Print (Reference "i")))
```

### `ForLoop`

> `ForLoop` represents the imperative programming c-style for loop:

#### Syntax

```haskell
(ForLoop Expression Expression Expression Expression)
```

#### C Example

```c
for (int i = 0; i < 10; i++) {
  foo(i);
}
```

```haskell
(ForLoop
  (Sequence [
    TypeSignature "i" (SimpleType "int" []),
    Variable "i" (MuNumber 0.0)])
  (Application (Primitive LessThan) [Reference "i",MuNumber 10.0])
  (Assignment "i" (Application (Primitive Plus) [Reference "i",MuNumber 1.0]))
  (Application (Reference "foo") [Reference "i"])))
```

#### JavaScript Example

```javascript
for (let i = 0; i < 10; i++) {
  console.log(i);
}
```

```haskell
(ForLoop
  (Variable "i" (MuNumber 0.0))
  (Application (Reference "<") [Reference "i",MuNumber 10.0])
  (Assignment "i" (Application (Reference "+") [Reference "i",MuNumber 1.0]))
  (Send (Reference "console") (Reference "log") [Reference "i"]))
```

### `Sequence`

> Generic sequence of statements

#### Syntax

```haskell
(Sequence [Expression])
```

### `Other`

> Unrecognized expression, with optional description and body

#### Syntax

```haskell
(Other (Maybe Code) (Maybe Expression))
```

### `Arrow`

> Generic arrow - AKA pair or entry - that is typically used to build dictionaries.
> It corresponds to ruby's, perl's and php's `=>` operator, or ruby's and javascript's `:` operator

#### Syntax

```haskell
(Arrow Expression Expression)
```

See [MuDict](#mudict) for more details

### `Self`

> Object oriented self-reference, like  C-like `this` and Smalltalk-derived `self`

#### Syntax

```haskell
(Self)
```

### `None`

> Used as a placeholder for empty bodies.

#### Syntax

```haskell
(None)
```

### `Break`

> Used to break out of flow structure

#### Syntax

```haskell
(Break Expression)
```

### `Continue`

> Used to jump over to next flow structure step

#### Syntax

```haskell
(Continue Expression)
```

### `MuNil`

> Generic nothing value literal - `nil`, `null`, `()` or `unit`.

#### Syntax

```haskell
(MuNil)
```

### `MuDict`

> Generic dictionary - AKA hash, table or map - value literal.
> Its expressions are normally a sequence of `Arrow`s

##### Syntax

```haskell
(MuDict Expression)
```
#### Python Example

```python
{'foo': 1}
```

```haskell
(MuDict (Arrow (MuString "foo") (MuNumber 1)))
```

```python
{'foo': 1, 'bar': 2}
```

```haskell
(MuDict (Sequence [
  (Arrow (MuString "foo") (MuNumber 1)),
  (Arrow (MuString "bar") (MuNumber 2))])
```

### `MuObject`

> Object oriented unnamed object literal

#### Syntax

```haskell
(MuObject Expression)
```

#### JavaScript Example

```javascript
{}
{foo: 1}
{foo: 1, bar: 2}
```

```haskell
(MuObject MuNil)
(MuObject (Attribute "foo" (MuNumber 1)))
(MuObject (Sequence [
            (Attribute "foo" (MuNumber 1)),
            (Attribute "bar" (MuNumber 2))]))
```

### `MuNumber`, `MuBool`, `MuString`, `MuSymbol` and `MuChar`

> Generic number, boolean, string, symbol (atoms) and char literals

#### Syntax

```haskell
(MuNumber Double)
```

```haskell
(MuBool Bool)
```

```haskell
(MuString String)
```

```haskell
(MuSymbol String)
```

```haskell
(MuChar Char)
```

#### Ruby Example

```ruby
1
true
"hello"
:hello
```

```haskell
(Sequence [
  (MuNumber 1),
  (MuBool True),
  (MuString "hello"),
  (MuSymbol "hello")])
```

### `MuTuple` and `MuList`

> They represent tuples - generic non-uniform fixed-size collection of elements - and lists - generic uniform variable-size collection of elements.
> Lists typically map to arrays, lists or sequence-like structures.

#### Syntax

```haskell
(MuTuple [Expression])
```

```haskell
(MuList [Expression])
```

### `TestGroup`, `Test` and `Assert`

> Generic test framework expressions used to represent unit tests.
> TestGroup represents a test grouping expression such as `describe`, `context`, etc
> Test represents a test expression such as `it`, etc
> Assert represents a test's assertion, such as `assert.equals(...)`, etc. It receives a boolean that represents whether the assertion is negated or not.

#### Syntax

```haskell
(TestGroup Expression Expression)
```

```haskell
(Test Expression Expression)
```

```haskell
(Assert Bool Assertion)
```

#### Javascript Example

```javascript
describe("succ", function() {
  it("succ of 3 is 4", function() {
    assert.equals(succ(3), 4)
  })
})
```

```haskell
TestGroup (MuString "succ")
  (Test (MuString "succ of 3 is 4")
    (Assert False (Equality (Application (Reference "succ") [MuNumber 3.0]) (MuNumber 4.0))))
```

#### Python Example

```python
class TestGroup(unittest.TestCase):
  def test_succ_of_3_is_4():
    self.assertEqual(succ(3), 4)
```

```haskell
TestGroup (MuString "TestGroup")
  (Test (MuString "test_succ_of_3_is_4")
    (Assert False (Equality (Application (Reference "succ") [MuNumber 3.0]) (MuNumber 4.0))))
```

## Assertion

Assertions used within tests to dynamically ascertain the code's validity.

An assertion can be one of:
 * `Truth`: Assert the truthfulness of a given expression.
 * `Equality`: Assert the equality of two given expressions.
 * `Failure`: Assert a given expression fails with a given error.

#### Syntax

```haskell
(Truth Expression)
```

```haskell
(Equality Expression Expression)
```

```haskell
(Failure Expression Expression)
```

#### Javascript Examples

```javascript
assert(true)
```

```haskell
Assert False (Truth (MuBool True))
```

```javascript
assert.equals(3, 3)
```

```haskell
Assert False (Equality (MuNumber 3) (MuNumber 3))
```

```javascript
assert.throws(function() { throw('error!') }, 'error!')
```

```haskell
Assert False (Failure (Lambda [] (Raise (MuString "error!"))) (MuString "error!"))
```

## Patterns

Patterns are the second most important element of Mulang AST. They represent things that don't hold a value, but are instead used to match values, like
patterns in imperative `case` or `switch` statements, functional pattern matching in `match` or `case` expressions, or exception matching in `try-catch` or `begin-rescue`-like statements in object oriented languages.

### `VariablePattern`

> Variable pattern represent a variable match. It corresponds to normal formal parameters in precedural languages,
> and to simple pattern matching against a free identifier.

#### Syntax

```haskell
(VariablePattern String)
```

#### JavaScript Example

```javascript
function foo(x, y) { }
```

```haskell
(Function "foo"
  [(Equation
      [(VariablePattern "x"), (VariablePattern "y")]
      (UnguardedBody MuNil))])
```

### `LiteralPattern`

> Literal constant pattern

#### Syntax


```haskell
(LiteralPattern String)
```

#### Example

### `InfixApplicationPattern`

> Infix application pattern like `4:X`

#### Syntax


```haskell
(InfixApplicationPattern Pattern String Pattern)
```
##### Caveats

`InfixApplicationPattern` exposes the underying syntax and will be deprecated.

### `ApplicationPattern`

> prefix application pattern like `f _`

#### Syntax


```haskell
(ApplicationPattern String [Pattern])
```

#### Example

### `TuplePattern`

> tuple pattern like `(3, _)`

#### Syntax


```haskell
(TuplePattern [Pattern])
```

#### Example

### `ListPattern`

> list pattern like `[x, y, _]`

#### Syntax


```haskell
(ListPattern [Pattern])
```

#### Example

### `FunctorPattern`

> Prolog-like functor pattern, like `f(X, 6)`.

#### Syntax

```haskell
(FunctorPattern Identifier [Pattern])
```

#### Example

### `AsPattern`

#### Syntax

```haskell
(AsPattern Identifier Pattern)
```

#### Example

### `TypePattern`

> A type pattern, like in exception handling constructs in most object-oriented languages

#### Syntax

```haskell
(TypePattern Identifier)
```

#### Example

### `WildcardPattern`

> Wildcard pattern, typically `_` in functional an logic programming languages.

#### Syntax

```haskell
(WildcardPattern)
```

### `UnionPattern`

#### Syntax

```haskell
(UnionPattern [Pattern])
```

### `OtherPattern`

> Other unrecognized pattern

#### Syntax

```haskell
(OtherPattern)
```

## Primitive Operators

Primitive operators represent low-level language operations that are well known and common to most languages, usually in the fashion of operators.

* `Equal`: equal operator
* `NotEqual`: distinct operator
* `Negation`: not operator
* `And`: and operator
* `Or`: or operator
* `Hash`: hashcode operator
* `GreatherOrEqualThan`
* `GreatherThan`
* `LessOrEqualThan`
* `LessThan`
* `Otherwise`: guard's otherwise operator
* `Plus`
* `Minus`
* `Multiply`
* `Divide`
* `ForwardComposition`: `(f >> g)(x) = (g . f)(x) = g(f(x))` operator
* `BackwardComposition`: `(g << f)(x) = (g . f)(x) = g(f(x))` operator
* `Mod`
* `BitwiseOr`
* `BitwiseAnd`
* `BitwiseXor`
* `BitwiseLeftShift`
* `BitwiseRightShift`

## Types

When processing statically-typed languages, all type-information - regardless we are typing a function, a variable or a class - is represented with the `Type` ADT, can be one of:

  * `SimpleType`: composed by a type identifier and zero or type more constraints
  * `ParameterizedType`: composed by input type parmaters, an output type, and type constratins
  * `ConstrainedType`: composed by just type constraints.
  * `OtherType`: an unrecognized type

`Type`s can be introduced in the Mulang AST using the following elements:

### `TypeAlias`

> A `TypeAlias` represents a synonym for a type, like the `type` declaration in Haskell and Scala or C's `typedef`.
> It is a typical statically typed functional programming feature.

#### Syntax

```haskell
(TypeAlias Identifier Identifier)
```

#### Haskell Example

```haskell
type Point = (Int, Int)
```

```haskell
(TypeAlias "Point" "(Int, Int)")
```

### TypeSignature

> A `TypeSignature` represents an explicit type annotation for a computation,
> variable or module, as you can find in Java or Haskell.

#### Syntax

```haskell
(TypeSignature Identifier Type)
```

#### Haskell Examples

Simple types:

```haskell
name :: String
```

```haskell
(TypeSignature "name" (SimpleType "String" []))
```

Simple types and constraints:

```haskell
f :: Num a => a
````

```haskell
(TypeSignature "f" (SimpleType "a" ["Num a"]))
```

Parameterized types:


```haskell
elem :: (Eq a, Foldable t) => a -> t a -> Bool
````

```haskell
(TypeSignature "elem" (ParameterizedType ["a", "t a"] "Bool" ["Eq a", "Foldable t"]))
```

#### Java Examples

In Java, as in most typed C-like languages, type signature and variable declarations are bound. This means that, for example, a local variable declaration will produce both a `TypeSignature` and a `Variable` expression.

Variable and attribute types:

```java
String name;
```

```haskell
(TypeSignature "name" (SimpleType "String" []))
```

Method types:

```java
String f() { return null; }
```

```haskell
(TypeSignature "f" (ParameterizedType [] "String" []))
```

Method types with type parameters:

```java
<A> A f() { return null; }
```

```haskell
(TypeSignature "f" (ParameterizedType [] "A" ["A"]))
```

Method types with type parameters and constraints:

```java
<A super B> void f(A a) {}
```

```haskell
(TypeSignature "f" (ParameterizedType ["A"] "void" ["A super B"]))
```

Class or interfaces types:

```java
class A<B extends C, D extends C> { }
```

```haskell
(TypeSignature "A" (ConstrainedType ["B extends C", "D extends C"]))
```


### `TypeCast`

> A `TypeCast` represent explictly giving a type to an expression
> which may have static or dynamic impact on the program. It is aimed to represent
> type-casts in c-like languages and inline type signatures in functional languages.

#### Syntax

```haskell
(TypeCast Expression Type)
```

#### Haskell Examples

Simple types:

```haskell
... = 4 :: Num a => a
```

```haskell
(TypeCast (MuNumber 4) (SimpleType "a" ["Num a"]))
```

#### Java Examples

Variable and attribute types:

```java
(Integer) 4;
```

```haskell
(TypeCast (MuNumber 4) (SimpleType "Integer" []))
```

```java
(Option<Integer>) something;
```

```haskell
(TypeCast (Reference "something") (SimpleType "Option<Integer>" []))
```

##### Caveats

The type constraints refer to type-constrained parametrizations that the cast introduces, and
not any other kind of constraints the cast uses. That is whay the following Java code:

```java
(Num<A>) something;
```

produces:

```haskell
(TypeCast (Reference "something") (SimpleType "Num<A>" []))
```

instead of:

```haskell
(TypeCast (Reference "something") (SimpleType "Num" ["A"]))
```
