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

#### Expectations checking

```javascript
const code = mulang.nativeCode("JavaScript", "x = 1");

code.expect("*", "Assigns:x");
// => true

code.expect("*", "Assigns:y");
// => false

code.expect("*", "Assigns:x:WithNumber:1");
// => true

code.customExpect(`
  expectation "assigns 1":
    assigns with 1;
  expectation "assigns 2":
    assigns with 2`);
// => [['assigns 1', true], ['assigns 2', false]])
```

#### AST generation

```javascript
const code = mulang.nativeCode("Python", "x = 1");

code.ast
// => { tag: 'Assignment', contents: [ 'x', { tag: 'MuNumber', contents: 1 } ] }
```

#### Run analysis

```javascript
const code = mulang.nativeCode("Python", "x = 1");

code.analyse({expectations: [{binding: '*', inspection: 'Declares'}, {binding: '*', inspection: 'Assigns'}]}).expectationResults
// => [
//  { expectation: { binding: '*', inspection: 'Declares' }, result: false },
//  { expectation: { binding: '*', inspection: 'Assigns' },  result: true }
//]
```

#### Internationalization

```javascript
mulang.I18n.locale = 'en'

mulang.I18n.translate('*', 'Declares:foo')
// => 'solution must declare <strong>foo</strong>'
```

#### Raw mulang execution

```javascript
const mulang = require('mulang')
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
                }
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
