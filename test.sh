#!/bin/bash
set -e

echo "[Mulang] Building...."
./build.sh --fast

pushd gem

echo "[Mulang] Running rspec tests...."
rake

popd

echo "[Mulang] Running tests...."
stack test --fast $@
