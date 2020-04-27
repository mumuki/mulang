require 'yaml'


$tokens = YAML.load(File.read('./tokens.yml'))

## =============================
## Haskell Operators Generation
## =============================

def generate_haskell_tokens_list(tokens)
  if tokens.is_a? String
    "[\"#{tokens}\"]"
  else
    "[#{tokens.map { |it| "\"#{it}\"" }.join(', ')}]"
  end
end

def generate_operators_hs(language, language_module, operators)
  tuples = operators.map do |operator, tokens|
    "(#{operator}, #{generate_haskell_tokens_list tokens})"
  end.join(",\n    ")

  %Q{{-# LANGUAGE ViewPatterns #-}

module Language.Mulang.Operators.#{language_module} (#{language}TokensTable) where
import Language.Mulang.Operators (TokensTable, buildTokensTable)
import Language.Mulang.Ast.Operator (Operator (..))

#{language}TokensTable :: TokensTable
#{language}TokensTable = buildTokensTable [
    #{tuples}
  ]
}
end

$tokens.each do |language_module, values|
  language = language_module[0].downcase + language_module[1..-1]
  File.write "./src/Language/Mulang/Operators/#{language_module}.hs", generate_operators_hs(language, language_module, (values['operators'] || {}))
end

## ===========================
## Haskell Keywords Generation
## ===========================

## ====================
## Ruby I18n Generation
## ====================

## ==========================
## JavaScript I18n Generation
## ==========================

## ============================
## Ruby Tokens Table Generation
## ============================

def generate_ruby_tokens_list(values, kind)
  (values["#{kind}s"] || []).map do |key, value|
    token = value.is_a?(Array) ? value.first : value
    ["#{kind}_#{key}", token]
  end
end

def generate_tokens_rb(ruby_tokens)
  %Q{module Mulang
  module Tokens
    TOKENS = {
    #{ruby_tokens}
    }.transform_values { |v| CGI::escapeHTML(v) }.freeze

    DEFAULT_TOKENS = TOKENS[:Haskell].merge(TOKENS[:C]).freeze
  end
end}
end

File.write "./gem/lib/mulang/tokens.rb", generate_tokens_rb($tokens.map do |key, values|
  tokens = generate_ruby_tokens_list(values, 'keyword') + generate_ruby_tokens_list(values, 'operator')
  "  #{key}: {\n#{
    tokens.map { |k, v| "        #{k}: '#{v}'" }.join(",\n")
  }\n      }"
end.join(",\n    "))

## ==================================
## JavaScript Tokens Table Generation
## ==================================

