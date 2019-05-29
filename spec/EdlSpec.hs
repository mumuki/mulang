module EdlSpec (spec) where

import Test.Hspec hiding (Expectation)
import Language.Mulang.Edl
import Language.Mulang.Edl.Expectation


simple inspection binding = simpleMatching inspection binding Unmatching
simpleCount inspection binding count = Decontextualize (count (Counter inspection binding Unmatching))
simpleCountWithin scope inspection binding count = Within scope (count (Counter inspection binding Unmatching))

simpleNegated inspection binding = (Decontextualize (CNot (Inspection inspection binding Unmatching)))
simpleNegatedWithin scope inspection binding = (Within scope (CNot (Inspection inspection binding Unmatching)))
simpleNegatedThrough scope inspection binding = (Through scope (CNot (Inspection inspection binding Unmatching)))

simpleThrough scope inspection binding = (Through scope (Inspection inspection binding Unmatching))
simpleMatchingWithin scope inspection binding matcher = (Within scope (Inspection inspection binding matcher))
simpleMatching inspection binding matcher = (Decontextualize (Inspection inspection binding matcher))

spec :: Spec
spec = do
  describe "parseQuery" $ do
    let run = parseQuery :: String -> Query
    let test code expectation = it ("test " ++ code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "calls" (simple "calls" Any)
    test "calls `foo`" (simple "calls" (Named "foo"))
    test "calls something like `foo`" (simple "calls" (Like "foo"))
    test "calls something distinct of `foo`" (simple "calls" (Except "foo"))
    test "calls something in (`foo`, `bar`, `baz`)" (simple "calls" (AnyOf ["foo", "bar", "baz"]))

    test "calls something" (run "calls")

    test "! calls" (simpleNegated "calls" Any)
    test "! calls `foo`" (simpleNegated "calls" (Named "foo"))
    test "! calls something like `foo`" (simpleNegated "calls" (Like "foo"))
    test "! calls something distinct of `foo`" (simpleNegated "calls" (Except "foo"))
    test "! calls something in (`foo`, `bar`, `baz`)" (simpleNegated "calls" (AnyOf ["foo", "bar", "baz"]))

    test "through `foobar` calls" (simpleThrough "foobar" "calls" Any)
    test "through `foobar` calls `foo`" (simpleThrough "foobar" "calls" (Named "foo"))
    test "through `foobar` calls something like `foo`" (simpleThrough "foobar" "calls" (Like "foo"))
    test "through `foobar` calls something distinct of `foo`" (simpleThrough "foobar" "calls" (Except "foo"))
    test "through `foobar` calls something in (`foo`, `bar`, `baz`)" (simpleThrough "foobar" "calls" (AnyOf ["foo", "bar", "baz"]))

    test "declares class like `Foo`" (simple "declares class" (Like "Foo"))
    test "declares class distinct of `Foo`" (simple "declares class" (Except "Foo"))
    test "declares class in (`Foo`, `Bar`)" (simple "declares class" (AnyOf ["Foo", "Bar"]))
    test "declares method like `foo`" (simple "declares method" (Like "foo"))
    test "declares method distinct of `foo`" (simple "declares method" (Except "foo"))
    test "declares method in (`foo`, `bar`)" (simple "declares method" (AnyOf ["foo", "bar"]))

    test "through `foobar` ! calls" (simpleNegatedThrough "foobar" "calls" Any)
    test "through `foobar` ! calls `foo`" (simpleNegatedThrough "foobar" "calls" (Named "foo"))
    test "through `foobar` ! calls something like `foo`" (simpleNegatedThrough "foobar" "calls" (Like "foo"))
    test "through `foobar` ! calls something distinct of `foo`" (simpleNegatedThrough "foobar" "calls" (Except "foo"))
    test "through `foobar` ! calls something in (`foo`, `bar`, `baz`)" (simpleNegatedThrough "foobar" "calls" (AnyOf ["foo", "bar", "baz"]))

    test "within `bar` ! calls" (simpleNegatedWithin "bar" "calls" Any)
    test "within `bar` ! calls `foo`" (simpleNegatedWithin "bar" "calls" (Named "foo"))
    test "within `bar` ! calls something like `foo`" (simpleNegatedWithin "bar" "calls" (Like "foo"))
    test "within `bar` ! calls something distinct of `foo`" (simpleNegatedWithin "bar" "calls" (Except "foo"))
    test "within `bar` ! calls something in (`foo`, `bar`, `baz`)" (simpleNegatedWithin "bar" "calls" (AnyOf ["foo", "bar", "baz"]))

    test "count (declares class `Baz`) = 3" (simpleCount "declares class" (Named "Baz") (Exactly 3))

    test "not (within `bar` count (calls) = 3)" (Not (simpleCountWithin "bar" "calls" Any (Exactly 3)))

    test "(within `f` calls) or (within `g` returns)" (Or (Within "f" (Inspection "calls" Any Unmatching)) (Within "g" (Inspection "returns" Any Unmatching)))
    test "calls || returns" (Decontextualize (COr (Inspection "calls" Any Unmatching) (Inspection "returns" Any Unmatching)))
    test "(calls) || (returns)" (run "calls || returns")

    test "(within `f` calls) and (within `g` returns)" (And (Within "f" (Inspection "calls" Any Unmatching)) (Within "g" (Inspection "returns" Any Unmatching)))
    test "calls && returns" (Decontextualize (CAnd (Inspection "calls" Any Unmatching) (Inspection "returns" Any Unmatching)))
    test "(calls) && (returns)" (run "calls && returns")


    test "count(calls) >= 3 && count(returns) >= 4" (Decontextualize (CAnd (AtLeast 3 (Counter "calls" Any Unmatching)) (AtLeast 4 (Counter "returns" Any Unmatching))))
    test "(count(calls) >= 3) && (count(returns) >= 4)" (run "count(calls) >= 3 && count(returns) >= 4")

    test "within `bar` count (calls) = 3" (simpleCountWithin "bar" "calls" Any (Exactly 3))
    test "within `bar` count (calls `foo`) = 3" (simpleCountWithin "bar" "calls" (Named "foo") (Exactly 3))
    test "within `bar` count (calls something like `foo`) = 3" (simpleCountWithin "bar" "calls" (Like "foo") (Exactly 3))
    test "within `bar` count (calls something distinct of `foo`) = 3" (simpleCountWithin "bar" "calls" (Except "foo") (Exactly 3))
    test "within `bar` count (calls something in (`foo`, `bar`, `baz`)) = 3" (simpleCountWithin "bar" "calls" (AnyOf ["foo", "bar", "baz"]) (Exactly 3))

    test "within `bar` (calls `foo`)" (run "within `bar` calls `foo`")
    test "within `bar` (calls `foo`) || (calls `foo`)" (run "within `bar` calls `foo` || calls `foo`")
    test "within `bar` (calls `foo` || calls `foo`)" (run "within `bar` calls `foo` || calls `foo`")

    test "within `bar` calls `foo` || calls `baz`" (Within "bar" (COr (Inspection "calls" (Named "foo") Unmatching) (Inspection "calls" (Named "baz") Unmatching)))
    test "within `bar` calls `foo` && calls `baz`" (Within "bar" (CAnd (Inspection "calls" (Named "foo") Unmatching) (Inspection "calls" (Named "baz") Unmatching)))
    test "within `bar` count (calls `foo`) + count (calls `baz`) >= 2" (Within "bar" (AtLeast 2 (Plus (Counter "calls" (Named "foo") Unmatching) (Counter "calls" (Named "baz") Unmatching))))
    test "within `bar` calls `a` && calls `b` || calls `c`" (run "within `bar` (calls `a` && calls `b`) || calls `c`")
    test "within `bar` calls `a` && calls `b` || calls `c` || calls `d`" (run "within `bar` ((calls `a` && calls `b`) || calls `c`) || calls `d`")
    test "within `bar` calls `a` && calls `b` || calls `c` && calls `d`" (run "within `bar` (calls `a` && calls `b`) || (calls `c` && calls `d`)")

    test "declares body `foo` that (calls `baz` || calls `bar`)" (run "declares body `foo` with something that (calls `baz` || calls `bar`)")

    test "within `bar` count (calls) >= 3" (simpleCountWithin "bar" "calls" Any (AtLeast 3))
    test "within `bar` count (calls `foo`) >= 3" (simpleCountWithin "bar" "calls" (Named "foo") (AtLeast 3))
    test "within `bar` count (calls something like `foo`) >= 3" (simpleCountWithin "bar" "calls" (Like "foo") (AtLeast 3))
    test "within `bar` count (calls something distinct of `foo`) >= 3" (simpleCountWithin "bar" "calls" (Except "foo") (AtLeast 3))
    test "within `bar` count (calls something in (`foo`, `bar`, `baz`)) >= 3" (simpleCountWithin "bar" "calls" (AnyOf ["foo", "bar", "baz"]) (AtLeast 3))

    test "within `bar` count (calls) <= 3" (simpleCountWithin "bar" "calls" Any (AtMost 3))
    test "within `bar` count (calls `foo`) <= 3" (simpleCountWithin "bar" "calls" (Named "foo") (AtMost 3))
    test "within `bar` count (calls something like `foo`) <= 3" (simpleCountWithin "bar" "calls" (Like "foo") (AtMost 3))
    test "within `bar` count (calls something distinct of `foo`) <= 3" (simpleCountWithin "bar" "calls" (Except "foo") (AtMost 3))
    test "within `bar` count (calls something in (`foo`, `bar`, `baz`)) <= 3" (simpleCountWithin "bar" "calls" (AnyOf ["foo", "bar", "baz"]) (AtMost 3))

    test "within `bar` returns with 0" (simpleMatchingWithin "bar" "returns" Any (Matching [IsNumber 0]))
    test "within `bar` returns with \"hello\"" (simpleMatchingWithin "bar" "returns" Any (Matching [IsString "hello"]))
    test "within `bar` returns with `hello`" (simpleMatchingWithin "bar" "returns" Any (Matching [IsSymbol "hello"]))
    test "within `bar` returns with 'a'" (simpleMatchingWithin "bar" "returns" Any (Matching [IsChar 'a']))
    test "within `bar` returns with true" (simpleMatchingWithin "bar" "returns" Any (Matching [IsTrue]))
    test "within `bar` returns with false" (simpleMatchingWithin "bar" "returns" Any (Matching [IsFalse]))
    test "within `bar` returns with nil" (simpleMatchingWithin "bar" "returns" Any (Matching [IsNil]))
    test "within `bar` returns with self" (simpleMatchingWithin "bar" "returns" Any (Matching [IsSelf]))
    test "within `bar` returns with math" (simpleMatchingWithin "bar" "returns" Any (Matching [IsMath]))
    test "within `bar` returns with logic" (simpleMatchingWithin "bar" "returns" Any (Matching [IsLogic]))

    test "within `bar` calls `foo` with (0, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsNumber 0, IsSelf]))
    test "within `bar` calls `foo` with (\"hello\", self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsString "hello", IsSelf]))
    test "within `bar` calls `foo` with (`hello`, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsSymbol "hello", IsSelf]))
    test "within `bar` calls `foo` with ('a', self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsChar 'a', IsSelf]))
    test "within `bar` calls `foo` with (true, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsTrue, IsSelf]))
    test "within `bar` calls `foo` with (false, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsFalse, IsSelf]))
    test "within `bar` calls `foo` with (nil, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsNil, IsSelf]))
    test "within `bar` calls `foo` with (self, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsSelf, IsSelf]))
    test "within `bar` calls `foo` with (math, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsMath, IsSelf]))
    test "within `bar` calls `foo` with (logic, self)" (simpleMatchingWithin "bar" "calls" (Named "foo") (Matching [IsLogic, IsSelf]))

    test "declares function like `total` that (uses not)" (simpleMatching "declares function" (Like "total") (Matching [That (simple "uses not" Any)]))
    test "declares function like `total` that (uses logic)" (simpleMatching "declares function" (Like "total") (Matching [That (simple "uses logic" Any)]))
    test "declares function like `total` that (uses math)" (simpleMatching "declares function" (Like "total") (Matching [That (simple "uses math" Any)]))

    test "declares function like `total` that (returns that (uses math))" (
      simpleMatching "declares function" (Like "total") (Matching [That (
        simpleMatching "returns" Any (Matching [That (
          simple "uses math" Any)]))]))
    test "declares function like `total` that (returns something that (uses math))" (run "declares function like `total` that (returns that (uses math))")

    test "calls `foo` with something that (returns with math)" (
      simpleMatching "calls" (Named "foo") (Matching [That (simpleMatching "returns" Any (Matching [IsMath]) )]))

    test "declares `foo` that (returns with math)" (
      simpleMatching "declares" (Named "foo") (Matching [That (simpleMatching "returns" Any (Matching [IsMath]) )]))

    test "calls `foo` with (self, something that (returns with math))" (
      simpleMatching "calls" (Named "foo") (Matching [IsSelf, That (simpleMatching "returns" Any (Matching [IsMath]) )]))

    test "calls `foo` with (self, that (returns with math))" (
      simpleMatching "calls" (Named "foo") (Matching [IsSelf, That (simpleMatching "returns" Any (Matching [IsMath]))]))

    test "calls `foo` with (self, something that (declares method `baz`))" (
      simpleMatching "calls" (Named "foo") (Matching [IsSelf, That (simple "declares method" (Named "baz"))]))

  describe "parseExpectations" $ do
    let run = parseExpectations
    let test code expectation = it ("test " ++ code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "expectation: declares class `Baz`" [Expectation "E0" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching))]
    test "expectation: declares class `Baz`;" [Expectation "E0" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching))]
    test "expectation: declares class `Baz`;\n" [Expectation "E0" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching))]

    test "expectation: declares class `Baz`;\nexpectation: within `Baz` sends `foo`" [
      Expectation "E0" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching)),
      Expectation "E1" (Within "Baz" (Inspection "sends" (Named "foo") Unmatching))]
    test "expectation: declares class `Baz`;\nexpectation: within `Baz` sends `foo`;" [
      Expectation "E0" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching)),
      Expectation "E1" (Within "Baz" (Inspection "sends" (Named "foo") Unmatching))]
    test "expectation: declares class `Baz`;\n\
         \expectation: within `Baz`\n\
         \sends `foo`;\n" [
           Expectation "E0" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching)),
           Expectation "E1" (Within "Baz" (Inspection "sends" (Named "foo") Unmatching))]

    test "expectation \"a test\":\n\
         \  declares class `Baz`" [Expectation "a test" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching))]
    test "expectation \"a test\":\n\
         \  declares class `Baz`;" [Expectation "a test" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching))]
    test "expectation \"a test\":\n\
         \  declares class `Baz`;\n" [Expectation "a test" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching))]

    test "expectation \"a test\":\n\
         \  declares class `Baz`;\n\
         \expectation \"another test\":\n\
         \  within `Baz` sends `foo`" [
      Expectation "a test" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching)),
      Expectation "another test" (Within "Baz" (Inspection "sends" (Named "foo") Unmatching))]
    test "expectation \"a test\":\n\
         \  declares class `Baz`;\n\
         \expectation \"another test\":\n\
         \  within `Baz` sends `foo`;" [
      Expectation "a test" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching)),
      Expectation "another test" (Within "Baz" (Inspection "sends" (Named "foo") Unmatching))]
    test "expectation \"a test\":\n\
         \  declares class `Baz`;\n\
         \expectation \"another test\":\n\
         \  within `Baz`\n\
         \  sends `foo`;\n" [
           Expectation "a test" (Decontextualize (Inspection "declares class" (Named "Baz") Unmatching)),
           Expectation "another test" (Within "Baz" (Inspection "sends" (Named "foo") Unmatching))]

  describe "handles errors" $ do
    let run = either id (error.show) . parseExpectations'
    let test code expectation = it ("test " ++ code ++ " shouldBe " ++ show expectation) (run code `shouldBe` expectation)

    test "expectation: declares class `Baz" "Lexical error"
    test "expectation: declares function like `total` that (uses within)" "Parse Error: within is not expected here"
    test "expectation: declares class `Baz` exoctly 3 times" "Parse Error: Unexpected keyword exoctly"
    test "expectation: declares class `Baz`\n within `Baz` sends `foo`" "Parse Error: within is not expected here"
    test "expectation: declares class of distinct `Baz`\n" "Parse Error: of is not expected here"
    test "expectation: declares class distinct `Baz`\n" "Parse Error: symbol Baz is not expected here"
    test "expectation: declares class `Baz` 3 times" "Parse Error: number 3.0 is not expected here"
    test "expectation: declares class `Baz` ! = 3 times" "Parse Error: ! is not expected here"
    test "expectation: declares class `Baz`;\n\
         \expectation: Within `Baz`\n\
         \sends `foo`;\n" "Parse Error: Unexpected keyword sends"
    test "(calls) or (returns)" "Parse Error: Unexpected )"
    test "(count(calls) >= 3) and (count(returns) >= 4)" "Parse Error: Unexpected )"
    test "(calls) and (returns)" "Parse Error: Unexpected )"
