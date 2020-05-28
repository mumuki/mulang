#!/bin/bash
set -e

echo "[Mulang::Ghcjslib] Cleaning deploy folder..."
rm -rf .deploy/

if ! grep ghcjs stack.yaml -q; then
  echo "[Mulang::Ghcjslib] GHC compiler detected, running swap script..."
  ./ghcjslib/swap.sh
fi

echo "[Mulang::Ghcjslib] Building mulang with GHCJS..."
./ghcjslib/build.sh

mkdir -p .deploy/build
cp LICENSE ghcjslib/README.md ghcjslib/package.json .deploy/
cp ghcjslib/index.js .deploy/mulang-cli.js
cp ghcjslib/build/mulang.js .deploy/build/mulang.js

echo "[Mulang::Ghcjslib] Publishing package to npm..."
npm publish .deploy/
