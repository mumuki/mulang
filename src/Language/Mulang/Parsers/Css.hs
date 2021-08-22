module Language.Mulang.Parsers.Css (css, parseCss) where

import Language.Mulang.Ast
import Language.Mulang.Parsers
import Language.Mulang.Builder (compact)

import Text.CSS.Parser (parseBlocks)
import Data.Text (pack)

css :: Parser
css = compact . muTags . parseTree

parseCss :: MaybeParser
parseCss = Just . css

muTags :: [TagTree String] -> [Expression]
muTags = map muTag

muTag :: TagTree String -> Expression
muTag (TagBranch name attributes children) = Element name (muAttributes attributes) (muTags children)
muTag (TagLeaf (TagText string))           = MuString string
muTag (TagLeaf (TagOpen name attributes))  = Element name (muAttributes attributes) []

muAttributes :: [Attribute String] -> [(String, Expression)]
muAttributes = map $ \(name, value) -> (name, MuString value)