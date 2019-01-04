#!/bin/bash

if grep ghcjs stack.yaml -q; then
  echo "GHC compiler detected, running swap script..."
  ./ghcjslib/swap.sh
fi

echo "Building mulang with GHCJS..."
./ghcjslib/build.sh

echo "Publishing package to npm..."
npm publish ghcjslib
