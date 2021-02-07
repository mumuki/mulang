{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module HtmlSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Parsers.Html
import           Language.Mulang.Inspector.Literal (isOther)

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = html . unpack

spec :: Spec
spec = do
  describe "basic parsing" $ do
    it "parses simple tree" $ do
      html "<html><body><p></p></body></html>" `shouldBe` (Element "html" [] [Element "body" [] [Element "p" [] []]])

    it "parses tree with attributes" $ do
      html "<html><body id='wrap'><p class='hidden'></p></body></html>" `shouldBe` (
        Element "html" [] [
          Element "body" [("id", MuString "wrap")] [
            Element "p" [("class", MuString "hidden")] []]])

    it "parses tree with text nodes" $ do
      html "<html><body><p>hello world</p></body></html>" `shouldBe` (
        Element "html" [] [
          Element "body" [] [
            Element "p" [] [
              MuString "hello world"
            ]]])
