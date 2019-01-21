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
                  "content": "x = z + 1"
                },
                "spec": {
                  "expectations": [
                    {
                      "binding": "Intransitive:x",
                      "inspection": "Uses:z"
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
        "content" : "x = z + 1"
     },
     "spec" : {
        "expectations" : [
           {
              "binding" : "Intransitive:x",
              "inspection" : "Uses:z"
           }
        ],
        "smellsSet" : { "tag" : "NoSmells" }
     }
  }' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [
      {
         "expectation" : {
            "binding" : "Intransitive:x",
            "inspection" : "Uses:z"
         },
         "result" : true
      }
   ],
   "smells" : [],
   "intermediateLanguage" : null,
   "signatures" : []
}
```

## Deploying

Build and publish the package to NPM with:

```bash
$ ./ghcjslib/deploy.sh
```

## Try it out!

You can find an online interactive version of mulang, along with its documentation [here](http://mumuki.github.io/mulang/)
