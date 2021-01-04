#!/bin/bash
set -e

echo "[Mulang] Running generators...."
./generators/run

echo "[Mulang] Building with stack...."
stack build $@
