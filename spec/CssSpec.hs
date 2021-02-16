{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module CssSpec (spec) where

import           Test.Hspec
import           Language.Mulang.Ast
import           Language.Mulang.Parsers.Css
import           Language.Mulang.Inspector.Literal (isOther)

import           Data.Text (Text, unpack)
import           NeatInterpolation (text)

run :: Text -> Expression
run = css . unpack

spec :: Spec
spec = do
  describe "basic parsing" $ do
    it "parses simple tree" $ do
      css "p { }" `shouldBe` (Element "p" [] [])
