let jsonEditor;

document.addEventListener("DOMContentLoaded", function(event) {
  const examplesSelect = $(".examples-select");
  examplesSelect.change(function () {
    const selected = examplesSelect.val();
    const exampleJSON = examples[selected];
    jsonEditor.set(exampleJSON);
  });

  const container = document.getElementById("jsoneditor");
  const options = {
    mode: "code"
  };
  jsonEditor = new JSONEditor(container, options);
});

function analyse() {
  const result = mulang.analyse(jsonEditor.get());
  $("#result").jsonViewer(result);
}

const examples = {
  "intransitive": {
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
    }
  },
  "unscoped": {
    "sample": {
      "tag": "CodeSample",
      "language": "Haskell",
      "content": "x = 1"
    },
    "spec": {
      "expectations": [
        {
          "binding": "*",
          "inspection": "Declares:x"
        }
      ]
    }
  },
  "matchers": {
    "sample" : {
       "tag" : "CodeSample",
       "language" : "Python3",
       "content" : "print(\"bye\")\nexit(1)"
    },
    "spec" : {
       "expectations" : [
          {
             "binding" : "*",
             "inspection" : "Calls:exit:WithNumber:0"
          },
          {
             "binding" : "*",
             "inspection" : "Prints:WithString:\"bye\""
          }
       ]
    }
  },
  "custom": {
    "sample" : {
       "tag" : "CodeSample",
       "language" : "JavaScript",
       "content" : "function plusOne(x) { return x + 1 }"
    },
    "spec" : {
       "customExpectations" : "expectation: declares function `plusOne` that (returns with math);\nexpectation: !declares variable with literal"
    }
  },
  "signature": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y) { return x + y; }"
    },
    "spec": {
      "signatureAnalysisType": {
        "tag": "StyledSignatures",
        "style": "HaskellStyle"
      }
    }
  },
  "broken": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y { return x + y; }"
    },
    "spec": {
      "signatureAnalysisType": {
        "tag": "StyledSignatures",
        "style": "HaskellStyle"
      }
    }
  },
  "AST": {
    "sample": {
      "tag": "MulangSample",
      "ast": {
        "tag": "Sequence",
        "contents": [
          {
            "tag": "Variable",
            "contents": [
              "x",
              { "tag": "MuNumber", "contents": 1 }
            ]
          },
          {
            "tag": "Variable",
            "contents": [
              "y",
              { "tag": "MuNumber", "contents": 2 }
            ]
          }
        ]
      }
    },
    "spec": {
      "signatureAnalysisType": {
        "tag": "StyledSignatures",
        "style": "HaskellStyle"
      }
    }
  },
  "smellInclusion": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y) { return null; }"
    },
    "spec": {
      "smellsSet": {
        "tag": "NoSmells",
        "include": [
          "ReturnsNil",
          "DoesNullTest"
        ]
      }
    }
  },
  "smellExclusion": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y) { return null; }"
    },
    "spec": {
      "smellsSet": {
        "tag": "AllSmells",
        "exclude": [
          "ReturnsNil"
        ]
      }
    }
  },
  "expressiveness": {
    "sample": {
      "tag": "CodeSample",
      "language": "Prolog",
      "content": "son(Parent, Son):-parentOf(Son, Parent).parentOf(bart, homer)."
    },
    "spec": {
      "smellsSet": { "tag": "AllSmells" },
      "domainLanguage": {
        "caseStyle": "SnakeCase",
        "minimumIdentifierSize": 4,
        "jargon": ["id"]
      }
    }
  },
  "typosSmells": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function pls(x, y) { return x + y; }"
    },
    "spec": {
      "expectations": [
        { "binding": "*", "inspection": "Declares:plus" }
      ],
      "smellsSet": {
        "tag": "NoSmells",
        "include": [ "HasDeclarationTypos" ]
      }
    }
  },
  "intermediate": {
    "sample" : {
       "tag" : "CodeSample",
       "language" : "JavaScript",
       "content" : "function foo(x, y) { return null; }"
    },
    "spec" : {
       "includeOutputAst" : true
    }
  },
  "normalization": {
    "sample": {
      "tag": "MulangSample",
      "normalizationOptions": {
        "insertImplicitReturn": true
      },
      "ast": {
        "tag": "Procedure",
        "contents": [
          "foo",
          [
            [
              [
                {
                  "tag": "VariablePattern",
                  "contents": "x"
                }
              ],
              {
                "tag": "UnguardedBody",
                "contents": {
                  "tag": "Application",
                  "contents": [
                    {
                      "tag": "Primitive",
                      "contents": "Multiply"
                    },
                    [
                      {
                        "tag": "MuNumber",
                        "contents": 2
                      },
                      {
                        "tag": "Reference",
                        "contents": "x"
                      }
                    ]
                  ]
                }
              }
            ]
          ]
        ]
      }
    },
    "spec": {
      "includeOutputAst": true
    }
  },
  "testRunning" : {
    "sample" : {
       "tag" : "CodeSample",
       "language" : "JavaScript",
       "content" : "function f(x) { return x + 1 }"
    },
    "spec" : {
       "testAnalysisType" : {
         "tag" :  "ExternalTests",
         "test" : {
           "tag" : "CodeSample",
           "language" : "JavaScript",
           "content" : "it(\"f increments by one\", function() { assert.equals(f(1), 2) })"
         }
       }
     }
    }
};
