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
  },
  "unscoped": {
    "sample": {
      "tag": "CodeSample",
      "language": "Haskell",
      "content": "x = 1"
    },
    "spec": {
      "smellsSet": { "tag": "NoSmells" },
      "expectations": [
        {
          "binding": "*",
          "inspection": "Declares:x"
        }
      ]
    }
  },
  "signature": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y) { return x + y; }"
    },
    "spec": {
      "expectations": [],
      "smellsSet": { "tag": "NoSmells" },
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
      "expectations": [],
      "smellsSet": { "tag": "NoSmells" },
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
      "smellsSet": {
        "tag": "NoSmells"
      },
      "signatureAnalysisType": {
        "tag": "StyledSignatures",
        "style": "HaskellStyle"
      },
      "expectations": []
    }
  },
  "smellInclusion": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y) { return null; }"
    },
    "spec": {
      "expectations": [],
      "smellsSet": {
        "tag": "NoSmells",
        "include": [
          "ReturnsNil",
          "DoesNullTest"
        ]
      },
      "signatureAnalysisType": {
        "tag": "StyledSignatures",
        "style": "HaskellStyle"
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
      "expectations": [],
      "smellsSet": {
        "tag": "AllSmells",
        "exclude": [
          "ReturnsNil"
        ]
      },
      "signatureAnalysisType": {
        "tag": "StyledSignatures",
        "style": "HaskellStyle"
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
      "expectations": [],
      "smellsSet": { "tag": "AllSmells" },
      "domainLanguage": {
        "caseStyle": "SnakeCase",
        "minimumIdentifierSize": 4,
        "jargon": ["id"]
      }
    }
  },
  "intermediate": {
    "sample": {
      "tag": "CodeSample",
      "language": "JavaScript",
      "content": "function foo(x, y) { return null; }"
    },
    "spec": {
      "expectations": [],
      "smellsSet": { "tag": "AllSmells" },
      "domainLanguage": { "dictionaryFilePath": "/usr/share/dict/words" }
    }
  },
};
