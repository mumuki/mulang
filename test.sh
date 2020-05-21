#!/bin/bash
set -e

echo "[Mulang] Building...."
./build.sh

echo "[Mulang] Running tests...."
stack test --fast $@
