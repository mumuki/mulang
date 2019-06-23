# Building mulang from source

## Setup

To generate `mulang` executable, you have to build the project using [stack](https://haskellstack.org):

1. Install stack: `wget -qO- https://get.haskellstack.org/ | sh`
2. Go to the mulang project directory and setup it: `stack setup`
3. Build the project: `stack build`

## Installing and creating an executable


```bash
$ stack install
$ mulang
```

That will generate a `mulang` executable in the folder `~/.local/bin`.

## Running tests

```bash
$ stack test --fast
```

## Watching changes


```bash
$ stack test --fast --file-watch
```

## Loading mulang in the REPL

```bash
stack ghci
```

# Ruby wrapper

This module can also be deployed a ruby gem. `mulang` works with Ruby 2.3.1

```bash
cd gem
rake wrapper:wrap
bundle install
bundle exec rspec
```

See `gem/README` for more details.

# JavaScript library

`mulang` can also be compiled to JavaScript library using [ghcjs](https://github.com/ghcjs/ghcjs) and [ghcjslib](https://github.com/flbulgarelli/ghcjslib), which allows you to use it from `node` or the browser.

> :warning: you will need `node >= 7` installed on your system. If you have `nvm`, before starting run the following:
>
> ```sh
> $ nvm use $(cat ghcjslib/.nvmrc)
>```

1. Run `ghcjslib/swap.sh` for swapping to GHCJS compiler
2. Run `ghcjslib/build.sh` for building the `ghcjslib` release. It will be placed on `ghcjslib/build/mulang.js`
3. Run `ghcjslib/test.sh` for running both mocha and hspec tests.
4. Load it:
   1. in the browser: `google-chrome ghcjslib/index.html`
   2. in `node`: run `node`, and then, within the interpreter, run: `let mulang = require('./ghcjslib/build/mulang.js');`
5. Try it: `mulang.analyse(...pass here a spec as described in the README....)`
6. Run `ghcjslib/swap.sh` again for swapping back to ghc

# Tagging and releasing

```bash
./tag.sh
```

# Updating docs

These site is build using `mkdocs >= 0.17`. You can install it using  `pip`:

```bash
$ pip install mkdocs
```

From the project root folder, running `mkdocs serve` will serve the files in a local server, `mkdocs build` will build the static site to a foldes called `site`, and deploys to the `gh-pages` branch are done by running `mkdocs gh-deploy` directly.
