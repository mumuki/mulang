{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module UnjavaSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Parsers.Java (java)
import           Language.Mulang.Unparsers.Java (unparseJava)

shouldRoundTrip expression =  (java.unparseJava) expression `shouldBe` expression

spec :: Spec
spec = do
  let wrapStatement expr = (Class "Sample" Nothing
                              (Sequence [
                                SubroutineSignature "sample" [] "void" [],
                                SimpleMethod "sample" [] expr
                              ]))
  let wrapExpression expr = wrapStatement (Return expr)

  describe "unparse" $ do
    let itWorksWith expr expectedCode = it (show expr) (unparseJava expr `shouldBe` expectedCode)
    let itWorksWithJavaExpression expr expectedCode = it (show expr) (unparseJava (wrapExpression expr) `shouldBe` expectedCode)
    let itWorksWithJavaStatement expr expectedCode = it (show expr) (unparseJava (wrapStatement expr) `shouldBe` expectedCode)

    itWorksWithJavaExpression (MuNumber 1.5) "public class Sample { \tpublic void sample (  ) { return 1.5 ; }\n }"
    itWorksWithJavaStatement ((Assignment "x" (Send (Reference "x") (Reference "+") [MuNumber 8]))) "public class Sample { \tpublic void sample (  ) { x = (x + 8) ; }\n }"
    itWorksWithJavaStatement (Sequence [Print (MuString "hello"), Print (MuString "world")]) "public class Sample { \tpublic void sample (  ) { System.out.println(\"hello\");\n\tSystem.out.println(\"world\");\n\t }\n }"
    itWorksWithJavaStatement (Sequence [Print (MuString "hello"), SimpleSend Self "foo" []]) "public class Sample { \tpublic void sample (  ) { System.out.println(\"hello\");\n\tthis.foo();\n\t }\n }"

    itWorksWithJavaStatement (While MuTrue None) "public class Sample { \tpublic void sample (  ) { while ( true ) {  } }\n }"
    itWorksWithJavaStatement (While MuTrue (SimpleSend Self "foo" [])) "public class Sample { \tpublic void sample (  ) { while ( true ) { \tthis.foo();\n\t } }\n }"

  describe "roundTrip" $ do
    let itWorksWith expr = it (show expr) (shouldRoundTrip expr)
    let itWorksWithJavaExpression expr = it (show expr) (shouldRoundTrip (wrapExpression expr))
    let itWorksWithJavaStatement expr = it (show expr) (shouldRoundTrip (wrapStatement expr))

    describe "literals" $ do
      itWorksWithJavaExpression (MuNumber 1.5)
      itWorksWithJavaExpression (MuNumber 1)
      itWorksWithJavaExpression MuTrue
      itWorksWithJavaExpression (MuString "some string")

    itWorksWithJavaStatement (Assignment "one" (MuNumber 1))
    itWorksWithJavaExpression ((Reference "x"))
    itWorksWithJavaExpression ((Send Self (Reference "f") [MuNumber 2]))
    itWorksWithJavaExpression ((Send (Reference "o") (Reference "f") [(MuNumber 2)]))
    itWorksWithJavaStatement ((Assignment "x" (Send (Reference "x") (Reference "+") [MuNumber 8])))
    itWorksWithJavaExpression ((Send (Reference "x") (Reference "+") [Reference "y"]))
    itWorksWithJavaStatement (Sequence [Print (MuString "hello"), Print (MuString "world")])
    itWorksWithJavaStatement (Sequence [Print (MuString "hello"), SimpleSend Self "foo" []])
    itWorksWithJavaExpression ((Send MuTrue (Primitive Negation) []))

    describe "classes" $ do
      itWorksWith (Class "DerivedClassName" Nothing None)
      itWorksWith (Class "DerivedClassName" (Just "BaseClassName") None)
      itWorksWith (Class "Foo" Nothing None)
      itWorksWith (Class "Foo" (Just "Bar") None)

    --itWorksWithJavaStatement (If MuTrue (MuNumber 1) (If MuFalse (MuNumber 2) (MuNumber 3)))
    -- itWorksWith (Class "Sample" Nothing (SimpleMethod "foo" [VariablePattern "param"] (Print (Reference "param"))))
    itWorksWithJavaStatement (While MuTrue None)
    -- itWorksWithJavaStatement (For [Generator (TuplePattern [VariablePattern "x"]) (Application (Reference "range") [MuNumber 0, MuNumber 3])] None)
    -- itWorksWithJavaStatement (Raise (Application (Reference "Exception") [MuString "something"]))

    -- describe "lambdas" $ do
    --   itWorksWith $ (Lambda [VariablePattern "x"] (MuNumber 1))
    --   itWorksWith $ (Lambda [] (Reference "foo"))

    describe "boolean operations" $ do
      let muand x y = (Send x (Primitive And) [y])
      let muor  x y = (Send x (Primitive Or) [y])
      let muneg x = (Send x (Primitive Negation) [])

      itWorksWithJavaExpression $ (Reference "a") `muand` (Reference "b")
      itWorksWithJavaExpression $ (Reference "a") `muor` (Reference "b")
      itWorksWithJavaExpression $ (muneg (Reference "a")) `muor` (Reference "b")
      itWorksWithJavaExpression $ muneg ((Reference "a") `muor` (Reference "b"))
      itWorksWithJavaExpression $ muneg ((Reference "a") `muand` (Reference "b")) `muor` (Reference "c")
      itWorksWithJavaExpression $ (Reference "a") `muand` ((Reference "b") `muor` (Reference "c"))

    describe "interface" $ do
      itWorksWith (Interface "Foo" [] None)
      itWorksWith (Interface "Foo" [] (SubroutineSignature "foo" [] "void" []))
      itWorksWith (Interface "Foo" [] (SubroutineSignature "foo" [] "A" ["A"]))
      itWorksWith (Interface "Foo" [] (SubroutineSignature "foo" [] "int" []))
      itWorksWith (Interface "Foo" [] (Sequence [
                                  SubroutineSignature "foo" [] "void" [],
                                    SubroutineSignature "bar" [] "int" []]))
      itWorksWith (Interface "Foo" [] (SubroutineSignature "foo" ["String", "int"] "void" []))
      itWorksWith (Interface "Foo" ["Bar", "Baz"] None)

      -- shouldRoundTrip (Class "Foo" Nothing (Sequence [
      --                    TypeSignature "hello" (ParameterizedType [] "void" []),
      --                    SimpleMethod "hello" [] None
      --                 ]))
