var should = require('should');
let mulang = require('../build/mulang');

describe("mulang", () => {
  it("can compute analysis",  () => {
    const code = mulang.nativeCode("Python", "x = 1\ndef increment():\n\tx += 1");
    const result = code.analyse({
      expectations: [
        {binding: '*', inspection: 'Declares'},
        {binding: '*', inspection: 'Assigns'},
        {binding: '*', inspection: 'DeclaresProcedure:Increment'}],
      smellsSet: { tag: 'AllSmells' }})

    should(result.expectationResults).eql([
      { expectation: { binding: '*', inspection: 'Declares' }, result: true },
      { expectation: { binding: '*', inspection: 'Assigns' }, result: true },
      { expectation: { binding: '*', inspection: 'DeclaresProcedure:Increment' }, result: false }])
    should(result.smells).eql([{ binding: 'increment', inspection: 'HasDeclarationTypos:Increment' } ])
  })

  it("can run quick queries over native code", () => {
    const code = mulang.code("JavaScript", "x = 1");

    should(code.expect("*", "Assigns:x")).be.true()
    should(code.expect("*", "Assigns:y")).be.false()
    should(code.expect("*", "Assigns:x:WithLiteral")).be.true()
    should(code.expect("*", "Assigns:x:WithNumber:1")).be.true()
    should(code.expect("*", "Assigns:x:WithNumber:2")).be.false()
  });

  it("can compute ast",  () => {
    const nativeCode = mulang.nativeCode("Python", "print(4)");

    should(nativeCode.ast).eql({ tag: 'Print', contents: { tag: 'MuNumber', contents: 4 } });
  })

  it("can run quick queries over ast code", () => {
    const nativeCode = mulang.code("JavaScript", "x = 1");
    const code = mulang.astCode(nativeCode.ast);

    should(code.expect("*", "Assigns:x")).be.true()
    should(code.expect("*", "Assigns:y")).be.false()
    should(code.expect("*", "Assigns:x:WithLiteral")).be.true()
    should(code.expect("*", "Assigns:x:WithNumber:1")).be.true()
    should(code.expect("*", "Assigns:x:WithNumber:2")).be.false()
  });

  it("can run quick custom queries over code", () => {
    const code = mulang.code("JavaScript", "x = 1");

    should(code.customExpect(`
  expectation "assigns 1":
    assigns with 1;
  expectation "assigns 2":
    assigns with 2`)).be.eql([['assigns 1', true], ['assigns 2', false]]);

  });

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
      "outputIdentifiers": null,
      "outputAst": null,
      "transformedAsts": null,
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
        "includeOutputAst": true
      }
    })).be.eql({
      "expectationResults": [],
      "outputIdentifiers": null,
      "outputAst": {
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
      "transformedAsts": null,
      "signatures": [],
      "smells": [],
      "tag": "AnalysisCompleted",
      "testResults": []
    });
  })
})
