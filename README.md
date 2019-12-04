[![Build Status](https://travis-ci.org/mumuki/mulang.svg?branch=master)](https://travis-ci.org/mumuki/mulang)

# Mulang

> A universal, multi-language, multi-paradigm code analyzer

Mulang is a tool for analysing source code, which is built on top of five main components:

  1. an [Abstract Semantic Tree](https://mumuki.github.io/mulang/astspec/), an intermediate language which allows to express the semantic - as opposed to syntatic - structure of a multi-paradigm program;
  2. a set of more than 90 [inspections](https://mumuki.github.io/mulang/inspections) for querying code querying code either explicitly - _expectations_ - or implicitlt - _smells_.
  3. an [Expectations Definition Language (EDL)](https://mumuki.github.io/mulang/edlspec), a language for defining custom expectations
  4. a [command line tool](https://mumuki.github.io/mulang/clispec/) for analysing both source code in many languages and Mulang's AST. This tool is distributed as both a `linux-amd64` binary and a JavaScript package. See [downloads section](https://github.com/mumuki/mulang/releases).
  5. higher level interfaces in [ruby](https://rubygems.org/gems/mulang) and [javascript](https://www.npmjs.com/package/mulang) that are easier to use and provides some additional capabilities like expectations parsing and automatic internationalized humanization.


Please the docs at [the Mulang site](https://mumuki.github.io/mulang/).

# Contributors

 * Franco Bulgarelli @flbulgarelli @ [Mumuki](https://mumuki.org)
 * Julian Berbel Alt @julian-berbel @ [Mumuki](https://mumuki.org)
 * Federico Lochbaum @FedeLochbaum @ [UNQ](http://www.unq.edu.ar/)
 * Lucas Traverso @ludat @ [10Pines](https://www.10pines.com)
