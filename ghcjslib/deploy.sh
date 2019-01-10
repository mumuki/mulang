#!/bin/bash
set -e

echo "Cleaning deploy folder..."
rm -rf .deploy/

if ! grep ghcjs stack.yaml -q; then
  echo "GHC compiler detected, running swap script..."
  ./ghcjslib/swap.sh
fi

echo "Building mulang with GHCJS..."
./ghcjslib/build.sh

mkdir -p .deploy/build
cp LICENSE ghcjslib/README.md ghcjslib/package.json .deploy/
cp ghcjslib/index.js .deploy/mulang-cli.js
cp ghcjslib/build/mulang.js .deploy/build/mulang.js

echo "Publishing package to npm..."
npm publish .deploy/
