var should = require('should');
let mulang = require('../build/mulang');

describe("mulang", () => {
  it("can do basic analyis", () => {
    should(mulang.analyse({
      "sample": {
        "tag": "CodeSample",
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
      "generatedAsts": [],
      "signatures": [],
      "smells": [],
      "tag": "AnalysisCompleted"
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
        "astsGenerationType": "RootExpressionAst"
      }
    })).be.eql({
      "expectationResults": [],
      "generatedAsts": [{
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
      }],
      "signatures": [],
      "smells": [],
      "tag": "AnalysisCompleted"
    });
  })
})
