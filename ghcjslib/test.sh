#!/bin/bash
set -e

echo '[Mulang:Ghcjslib] Building...'
./ghcjslib/build.sh

pushd ghcjslib

echo '[Mulang:Ghcjslib] Installing npm dependencies...'
npm install

echo '[Mulang:Ghcjslib] Running mocha tests...'
npm test

echo '[Mulang:Ghcjslib] Generating hspec tests...'
node -e '
let fs = require("fs");
let specs = fs.readdirSync("../spec")
              .filter((it) => !it.startsWith("Spec"))
              .map((it) => it.replace(".hs", ""));
console.log(`Specs detected: ${specs.join(",")}`)

let imports = specs.map((it) => `import qualified ${it}`).join("\n");
let descriptions = specs.map((it) => `  describe "${it}" ${it}.spec`).join("\n");

console.log("Writing Spec.hs");
fs.writeFileSync("../spec/Spec.hs",
`module Main where

import Test.Hspec
${imports}

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
${descriptions}
`);'

popd
echo '[Mulang:Ghcjslib] Running hspec tests...'
stack test

echo '[Mulang:Ghcjslib] Cleaning hspec tests...'
cat > spec/Spec.hs <<EOF
module Main where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = error "Don't run ghcjslib specs directly. Use ./ghcjslib/test.sh instead"
EOF

echo '[Mulang:Ghcjslib] Done'

