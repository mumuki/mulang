#!/bin/bash
set -e

GHCJSLIB_BUNDLE_NAME=mulang

GHCJSLIB_PATH=./ghcjslib
GHCJSLIB_BUILD_PATH=$GHCJSLIB_PATH/build
GHCJSLIB_SRC_PATH=$GHCJSLIB_PATH/src
GHCJSLIB_BUNDLE=$GHCJSLIB_BUILD_PATH/$GHCJSLIB_BUNDLE_NAME.js

STACK_BUILD_PATH=./.stack-work/dist/x86_64-linux/Cabal-1.24.2.0_ghcjs/build/$GHCJSLIB_BUNDLE_NAME/$GHCJSLIB_BUNDLE_NAME.jsexe/

function append2Lib() {
  echo ">> Appending $(basename $1)..."
  cat $1 >> $GHCJSLIB_BUNDLE
}

echo 'Building project...'
stack build

echo "Building $GHCJSLIB_BUNDLE..."
mkdir -p $GHCJSLIB_BUILD_PATH

echo ">> Cleaning $GHCJSLIB_BUNDLE and $GHCJSLIB_BUNDLE.zip"
rm -f $GHCJSLIB_BUNDLE
rm -f $GHCJSLIB_BUNDLE.zip

append2Lib $GHCJSLIB_SRC_PATH/header.js.part
append2Lib $STACK_BUILD_PATH/rts.js
append2Lib $STACK_BUILD_PATH/lib.js
append2Lib $STACK_BUILD_PATH/out.js
append2Lib $GHCJSLIB_SRC_PATH/binding.js
append2Lib $GHCJSLIB_SRC_PATH/main.js
append2Lib $GHCJSLIB_SRC_PATH/code.js
append2Lib $GHCJSLIB_SRC_PATH/tokens.js
append2Lib $GHCJSLIB_SRC_PATH/i18n.js
append2Lib $GHCJSLIB_SRC_PATH/footer.js.part

echo "Generating compressed bundle $GHCJSLIB_BUNDLE.zip..."
pushd $GHCJSLIB_BUILD_PATH && zip $GHCJSLIB_BUNDLE_NAME.js.zip $GHCJSLIB_BUNDLE_NAME.js ; popd

echo "Done"
