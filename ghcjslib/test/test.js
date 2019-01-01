var should = require('should');
let mulang = require('../build/mulang');

describe("mulang", () => {
  it("can do basic analyis", () => {
    should(mulang.analyse({
      "sample": {
        "tag": "CodeFragment",
        "language": "Haskell",
        "content": "x = 1"
      },
      "spec": {
        "expectations": [
          {
            "binding": ":Intransitive:x",
            "inspection": "Uses:*"
          }
        ],
        "smellsSet": { "tag": "NoSmells" }
      }
    })).be.eql({
      "expectationResults": [
        {
          "expectation": {
            "binding": ":Intransitive:x",
            "inspection": "Uses:*",
          },
          "result": true
        },
      ],
      "intermediateLanguage": null,
      "signatures": [],
      "smells": [],
      "tag": "AnalysisCompleted"
    });
  })

  it("it can generate ast", () => {
    should(mulang.analyse({
      "sample": {
        "tag": "CodeFragment",
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
      "tag": "AnalysisCompleted"
    });
  })
})
