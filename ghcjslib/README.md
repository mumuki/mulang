## Usage

### As a standalone program

```sh
$ node index.js '{
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

### As a node library

```javascript
let mulang = require('./build/mulang.js')
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



### In the browser

```html
<script language="javascript" src="build/mulang.js"></script>
<script>
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
<script/>
```
