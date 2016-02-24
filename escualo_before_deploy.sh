#!/bin/bash

cabal build
cd dist/build/mulang
git init
git add mulang
git commit -m "[Escualo::Travis] Deploy $TRAVIS_COMMIT" 
