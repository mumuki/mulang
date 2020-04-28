require 'yaml'

## ======
## Common
## ======

def generate_frontend_tokens_table(tokens, polyfills: false)
  tokens
    .transform_values do |values|
      polyfills ? values['polyfills'] : values
    end
    .compact
    .map do |key, values|
      tokens = generate_frontend_tokens_list(values, 'keyword') + generate_frontend_tokens_list(values, 'operator')
      "  #{key}: {\n#{
        tokens.map { |k, v| "        #{k}: '#{v}'" }.join(",\n")
      }\n      }"
    end
    .join(",\n    ")
end

def generate_frontend_tokens_list(values, kind)
  (values["#{kind}s"] || []).map do |key, value|
    token = value.is_a?(Array) ? value.first : value
    ["#{kind}_#{key}", token]
  end
end


$tokens = YAML.load(File.read('./tokens.yml'))
$frontend_tokens_table = generate_frontend_tokens_table($tokens)
$frontend_polyfills_table = generate_frontend_tokens_table($tokens, polyfills: true)

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

puts '[Mulang::Generator::Tokens] Generating Haskell Operator Tables...'
$tokens.each do |language_module, values|
  language = language_module[0].downcase + language_module[1..-1]
  destination = "./src/Language/Mulang/Operators/#{language_module}.hs"
  puts "  Generating #{destination}..."
  File.write destination, generate_operators_hs(language, language_module, (values['operators'] || {}))
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


def generate_tokens_rb(tokens, polyfills)
  %Q{module Mulang
  module Tokens
    TOKENS = {
    #{tokens}
    }.transform_values { |v| v.transform_values { |v| CGI::escapeHTML(v) } }.freeze

    POLYFILLS = {
    #{polyfills}
    }.transform_values { |v| v.transform_values { |v| CGI::escapeHTML(v) } }.freeze

    DEFAULT_TOKENS = TOKENS[:Common].merge(POLYFILLS[:Common]).freeze
  end
end}
end

puts '[Mulang::Generator::Tokens] Generating Ruby Tokens Table...'
File.write "./gem/lib/mulang/tokens.rb", generate_tokens_rb($frontend_tokens_table, $frontend_polyfills_table)

## ==================================
## JavaScript Tokens Table Generation
## ==================================

def generate_tokens_js(tokens, polyfills)
  %Q{(() => {
  const TOKENS = {
  #{tokens}
  }

  const POLYFILLS = {
  #{polyfills}
  }

  const DEFAULT_TOKENS = {};
  Object.assign(DEFAULT_TOKENS, TOKENS.Common);
  Object.assign(DEFAULT_TOKENS, POLYFILLS.Common);

  ghcjsExports.Tokens = {
    TOKENS = TOKENS,
    DEFAULT_TOKENS = DEFAULT_TOKENS
  };
})();}
end

puts '[Mulang::Generator::Tokens] Generating JavaScript Tokens Table...'
File.write "./ghcjslib/src/tokens.js", generate_tokens_js($frontend_tokens_table, $frontend_polyfills_table)
