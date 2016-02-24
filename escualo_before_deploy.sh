#!/bin/bash

cd dist/build/mulang
git init
git config user.email "travis@mumuki.org"
git config user.name "Travis CI"
git add mulang
git commit -m "[Escualo::Travis] Deploy $TRAVIS_COMMIT" 
