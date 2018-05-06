{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module PolymorphismSpec (spec) where

import           Test.Hspec
import           Language.Mulang
import           Language.Mulang.Inspector.ObjectOriented.Polymorphism
import qualified Language.Mulang.Parsers.Java as J (java)

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

java :: Text -> Expression
java = J.java . unpack

spec :: Spec
spec = do
  describe "usesStaticMethodOverload" $ do
    it "is true when two methods with the same name and different arity exist on the same class" $ do
      usesStaticMethodOverload (java [text|
          class Wallet {
            void deposit(BitcoinMoney amount) {}
            void deposit(RegularMoney amount, double conversion) {}
          }
        |]) `shouldBe` True

    it "is true when two methods with the same name and different types exist on the same class" $ do
      usesStaticMethodOverload (java [text|
          class Wallet {
            void deposit(BitcoinMoney amount) {}
            void deposit(RegularMoney amount) {}
          }
        |]) `shouldBe` True

    it "is false when there are no duplicated methods" $ do
      usesStaticMethodOverload (java [text|
          class Wallet {
            void deposit(BitcoinMoney amount) {}
            void withdraw(BitcoinMoney amount) {}
          }
        |]) `shouldBe` False

  describe "usesDynamicMethodOverload" $ do
    it "is true when two methods with the same name and different arity exist on the same class" $ do
      usesDynamicMethodOverload (java [text|
          class Wallet {
            void deposit(BitcoinMoney amount) {}
            void deposit(RegularMoney amount, double conversion) {}
          }
        |]) `shouldBe` True

    it "is false when two methods with the same name and different types exist on the same class" $ do
      usesDynamicMethodOverload (java [text|
          class Wallet {
            void deposit(BitcoinMoney amount) {}
            void deposit(RegularMoney amount) {}
          }
        |]) `shouldBe` False

    it "is false when there are no duplicated methods" $ do
      usesDynamicMethodOverload (java [text|
          class Wallet {
            void deposit(BitcoinMoney amount) {}
            void withdraw(BitcoinMoney amount) {}
          }
        |]) `shouldBe` False

  describe "usesTemplateMethod" $ do
    it "is true when an abstract method is uses in an abstract class" $ do
      usesTemplateMethod (java [text|
          abstract class Account {
            double balance;
            void extract(double amount) {
              checkBalance();
              balance -= amount;
            }
            abstract void checkBalance();
          }
          class NormalAccount {
            void checkBalance() {}
          }
          class PremiumAccount {
            void checkBalance() {}
          }
        |]) `shouldBe` True

    it "is false when no abstract method is used" $ do
      usesTemplateMethod (java [text|
          abstract class Account {
            double balance;
            void extract(double amount) {
              balance -= amount;
            }
            abstract void checkBalance();
          }
          class NormalAccount {
            void checkBalance() {}
          }
          class PremiumAccount {
            void checkBalance() {}
          }
        |]) `shouldBe` False

  describe "usesObjectComposition" $ do
    it "is true an interface attribute is declared" $ do
      usesObjectComposition (java [text|
          interface Light {
            void on();
          }
          class Room {
            Light light;
            void enter() {
              light.on();
            }
          }|]) `shouldBe` True

    it "is no attribute is declared" $ do
      usesObjectComposition (java [text|
          interface Light {
            void on();
          }
          class Room {
          }|]) `shouldBe` False

    it "is a primitive attribute is declared" $ do
      usesObjectComposition (java [text|
          interface Light {
            void on();
          }
          class Room {
            int size;
          }|]) `shouldBe` False

  describe "usesDyamicPolymorphism" $ do
    it "is True when uses" $ do
      usesDyamicPolymorphism (java [text|
          class Bird { void sing() {} }
          class Performer { void sing() {} }
          class Festival { void run(Object o) { o.sing(); } }|]) `shouldBe` True

    it "is False when there is just one implementor" $ do
      usesDyamicPolymorphism (java [text|
          class Bird { void sing() {} }
          class Festival { void run(Object o) { o.sing(); } }|]) `shouldBe` False

    it "is False when there is no user" $ do
      usesDyamicPolymorphism (java [text|
        class Bird { void sing() {} }
        class Performer { void sing() {} }|]) `shouldBe` False

    it "is False when not uses" $ do
      usesDyamicPolymorphism (java [text|
        class Sample { void aMethod() { throw new Exception(); } }|]) `shouldBe` False

  describe "usesStaticPolymorphism" $ do
    it "is True when there is an usage of an interface in an attribute implemented by two or more classes" $ do
      usesStaticPolymorphism (java [text|
            interface Singer {
              void sing();
            }
            class Bird implements Singer {
              void sing() {}
            }
            class Performer implements Singer {
              void sing() {}
            }
            class Festival {
              Singer o;
              void run() { o.sing(); }
            }|]) `shouldBe` True

    it "is True when there is an usage of an interface in a parameter implemented by two or more classes" $ do
      usesStaticPolymorphism (java [text|
            interface Singer {
              void sing();
            }
            class Bird implements Singer {
              void sing() {}
            }
            class Performer implements Singer {
              void sing() {}
            }
            class Festival {
              void run(Singer o) { o.sing(); }
            }|]) `shouldBe` True


    it "is False when there is an usage of an interface implemented by just one class" $ do
      usesStaticPolymorphism (java [text|
            interface Singer {
              void sing();
            }
            class Bird implements Singer {
              void sing() {}
            }
            class Festival {
              void run(Singer o) { o.sing(); }
            }|]) `shouldBe` False

    it "is False when there is no interace" $ do
      usesStaticPolymorphism (java [text|
            class Bird {
              void sing() {}
            }
            class Performer {
              void sing() {}
            }
            class Festival {
              void run(Singer o) { o.sing(); }
            }|]) `shouldBe` False

    it "is True even when there no message is sent to an attribute" $ do
      usesStaticPolymorphism (java [text|
            interface Singer {
              void sing();
            }
            class Bird implements Singer {
              void sing() {}
            }
            class Performer implements Singer {
              void sing() {}
            }
            class Festival {
              Singer o = new Bird();
              void run() {}
            }|]) `shouldBe` True

    it "is True even when there is no message is sent to a paramter" $ do
      usesStaticPolymorphism (java [text|
            interface Singer {
              void sing();
            }
            class Bird implements Singer {
              void sing() {}
            }
            class Performer implements Singer {
              void sing() {}
            }
            class Festival {
              void run(Singer o) {}
            }|]) `shouldBe` True

    it "is False when interface declares no method" $ do
      usesStaticPolymorphism (java [text|
            interface Singer {
            }
            class Bird implements Singer {
              void sing() {}
            }
            class Performer implements Singer {
              void sing() {}
            }
            class Festival {
              Singer o;
              void run() { }
            }|]) `shouldBe` False
