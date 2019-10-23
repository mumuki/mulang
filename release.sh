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
sed -i -r "s/version = \"${VERSION_REGEXP}\"/version = \"${NEW_VERSION}\"/"               app/Version.hs
sed -i -r "s/VERSION = \"${VERSION_REGEXP}/VERSION = \"${NEW_VERSION}/"                   gem/lib/mulang/version.rb
sed -i -r "s/MULANG_VERSION = \"${VERSION_REGEXP}/MULANG_VERSION = \"${NEW_VERSION}/"     gem/lib/mulang/version.rb
sed -i -r "s/\"version\": \"${VERSION_REGEXP}/\"version\": \"${NEW_VERSION}/"             ghcjslib/package.json
sed -i -r "s/VERSION = \"${VERSION_REGEXP}/VERSION = \"${NEW_VERSION}/"                   ghcjslib/gem/lib/mulangjs/version.rb
sed -i -r "s/MULANG_VERSION = \"${VERSION_REGEXP}/MULANG_VERSION = \"${NEW_VERSION}/"     ghcjslib/gem/lib/mulangjs/version.rb
sed -i -r "s/MULANG_VERSION=${VERSION_REGEXP}/MULANG_VERSION=${NEW_VERSION}/"             docs/devinit

echo "[Mulang] Running tests..."
stack test

echo "[Mulang] Running ghcjslib tests..."
./ghcjslib/swap.sh
./ghcjslib/test.sh

echo "[Mulang] Deploying to NPM..."
./ghcjslib/deploy.sh
./ghcjslib/swap.sh

echo "[Mulang] Commiting files..."
git commit mulang.cabal \
           app/Version.hs \
           gem/lib/mulang/version.rb \
           ghcjslib/package.json \
           ghcjslib/gem/lib/mulangjs/version.rb \
           docs/devinit -m "Welcome ${NEW_VERSION}!"

echo "[Mulang] Tagging v$NEW_VERSION..."
git tag "v${NEW_VERSION}"

echo "[Mulang] Pushing to github..."
git push origin HEAD --tags

echo "[Mulang] Pushed. Travis will deploy mulang binaries and gem"
