## Installing

### As a node library

```bash
npm install mulang
```

### As a CLI

```bash
npm install -g mulang
```

## Using

### As a node/webpack library

```javascript
let mulang = require('mulang')
mulang.analyse({
                "sample": {
                  "tag": "CodeSample",
                  "language": "Haskell",
                  "content": "x = 1"
                },
                "spec": {
                  "expectations": [
                    {
                      "binding": "Intransitive:x",
                      "inspection": "Uses:*"
                    }
                  ],
                  "smellsSet": { "tag": "NoSmells" }
                });
```

### As a CLI

```bash
$ mulangjs '{
     "sample" : {
        "tag" : "CodeSample",
        "language" : "Haskell",
        "content" : "x = 1"
     },
     "spec" : {
        "expectations" : [
           {
              "binding" : "Intransitive:x",
              "inspection" : "Uses:*"
           }
        ],
        "smellsSet" : { "tag" : "NoSmells" }
     }
  }'
{"signatures":[],"smells":[],"expectationResults":[{"expectation":{"binding":"Intransitive:x","inspection":"Uses:*"},"result":true}],"tag":"AnalysisCompleted","intermediateLanguage":null}
```

## Deploying

Build and publish the package to NPM with:

```bash
$ ./ghcjslib/deploy.sh
```
