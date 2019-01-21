var should = require('should');
let mulang = require('../build/mulang');

describe("mulang", () => {
  it("can do basic analyis", () => {
    should(mulang.analyse({
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
    })).be.eql({
      "expectationResults": [
        {
          "expectation": {
            "binding": "Intransitive:x",
            "inspection": "Uses:z",
          },
          "result": true
        },
      ],
      "intermediateLanguage": null,
      "signatures": [],
      "smells": [],
      "tag": "AnalysisCompleted",
      "testResults": []
    });
  })

  it("it can generate ast", () => {
    should(mulang.analyse({
      "sample": {
        "tag": "CodeSample",
        "language": "JavaScript",
        "content": "function foo(x, y) { return null; }"
      },
      "spec": {
        "expectations": [],
        "smellsSet": { "tag": "NoSmells" },
        "includeIntermediateLanguage": true
      }
    })).be.eql({
      "expectationResults": [],
      "intermediateLanguage": {
        "contents": [
          "foo",
          [
            [
              [
                {
                  "contents": "x",
                  "tag": "VariablePattern"
                },
                {
                  "contents": "y",
                  "tag": "VariablePattern"
                }
              ],
              {
                "contents": {
                  "contents": {
                    "contents": [],
                    "tag": "MuNil"
                  },
                  "tag": "Return"
                },
                "tag": "UnguardedBody"
              }
            ]
          ]
        ],
        "tag": "Function"
      },
      "signatures": [],
      "smells": [],
      "tag": "AnalysisCompleted",
      "testResults": []
    });
  })
})
