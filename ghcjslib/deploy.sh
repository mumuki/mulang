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

mkdir .deploy
cp LICENSE ghcjslib/README.md ghcjslib/package.json ghcjslib/index.js .deploy/
cp ghcjslib/build/mulang.js .deploy/mulang.js

echo "Publishing package to npm..."
npm publish .deploy/
