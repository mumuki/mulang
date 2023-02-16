#!/bin/bash
set -e

echo "[Mulang] Installing ruby development dependencies...."
bundle install --quiet

echo "[Mulang] Running generators...."
./generators/run

echo "[Mulang] Building with stack...."
stack build $@
