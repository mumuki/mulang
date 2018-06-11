#!/bin/bash
set -e

function swap2ghc() {
  echo ">> Swapping $1...."
  mv $1 $1.ghcjs
  mv $1.ghc $1
}

function swap2ghcjs() {
  echo ">> Swapping $1...."
  mv $1 $1.ghc
  mv $1.ghcjs $1
}

echo 'Going to swap compilers....'
if grep ghcjs stack.yaml -q; then
  echo 'GHCJS compiler detected. Swapping to GHC...'
  swap2ghc stack.yaml
  swap2ghc mulang.cabal
  swap2ghc app/Main.hs
  swap2ghc spec/Spec.hs
  swap2ghc spec/PythonSpec.hs
  swap2ghc src/Language/Mulang/Analyzer/SampleParser.hs
else
  echo 'GHC compiler detected. Swapping to GHCJS...'
  swap2ghcjs stack.yaml
  swap2ghcjs mulang.cabal
  swap2ghcjs app/Main.hs
  swap2ghcjs spec/Spec.hs
  swap2ghcjs spec/PythonSpec.hs
  swap2ghcjs src/Language/Mulang/Analyzer/SampleParser.hs
fi

echo 'Done'

