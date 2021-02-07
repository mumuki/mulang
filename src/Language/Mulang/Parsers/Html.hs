module Language.Mulang.Parsers.Html (html, parseHtml) where

import Language.Mulang.Ast
import Language.Mulang.Parsers
import Language.Mulang.Builder (compact)

import Text.HTML.TagSoup (Tag (..), Attribute (..))
import Text.HTML.TagSoup.Tree (TagTree (..), parseTree)

html :: Parser
html = compact . muTags . parseTree

parseHtml :: MaybeParser
parseHtml = Just . html

muTags :: [TagTree String] -> [Expression]
muTags = map muTag

muTag :: TagTree String -> Expression
muTag (TagBranch name attributes children) = Element name (muAttributes attributes) (muTags children)
muTag (TagLeaf (TagText string))           = MuString string
muTag (TagLeaf (TagOpen name attributes))  = Element name (muAttributes attributes) []

muAttributes :: [Attribute String] -> [(String, Expression)]
muAttributes = map $ \(name, value) -> (name, MuString value)