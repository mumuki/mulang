#!/bin/bash

set -e

NEW_VERSION=$1
VERSION_REGEXP='[0-9]+\.[0-9]+\.[0-9]+'
FULL_VERSION_REGEXP="^${VERSION_REGEXP}$"

if [[ ! $NEW_VERSION =~ $FULL_VERSION_REGEXP ]]; then
  echo "First param should be a version like X.X.X"
  exit 1
fi

echo "[Mulang] Updating version..."
sed -i -r "s/version:             ${VERSION_REGEXP}/version:             ${NEW_VERSION}/" mulang.cabal
sed -i -r "s/version = \"${VERSION_REGEXP}\"/version = \"${NEW_VERSION}\"/" app/Version.hs
sed -i -r "s/VERSION = \"${VERSION_REGEXP}/VERSION = \"${NEW_VERSION}/" gem/lib/mulang/version.rb
sed -i -r "s/MULANG_VERSION = \"${VERSION_REGEXP}/MULANG_VERSION = \"${NEW_VERSION}/" gem/lib/mulang/version.rb

echo "[Mulang] Running tests..."
stack test

echo "[Mulang] Commiting files..."
git commit mulang.cabal app/Version.hs gem/lib/mulang/version.rb -m "Welcome ${NEW_VERSION}!"

echo "[Mulang] Tagging v$NEW_VERSION..."
git tag "v${NEW_VERSION}"

echo "[Mulang] Pushing..."
git push origin HEAD --tags

echo "[Mulang] Pushed. Travis will do the rest"
