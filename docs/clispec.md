# Command Line Tool

You can also use Mulang from the Command Line, without having to interact with Haskell code. This tool allows to perform most common analysis out of the box by using a JSON spec. It supports five different kinds of analysis:

1. **Expectation analysis**: you can pass _inspections_ that will be tested against the provied program. Expectations answer questions like: _does the function X call the function Y?_ or _does the program use if's?_.
2. **Smell analysis**: instead of asking explcit questions to the program, the smells analysis implicitly runs specific inspections - that denote bad code - in orden to know if any of them is matched.
3. **Intermediate Language analysis**: you can ask the tool to generate the Mulang AST for a given source code.
4. **Signature analysis**: report the signatures of the computations present in source code.
5. **Test analysis**: run basic unit-like tests over the code.

## Examples

Let's see some usage samples:

### With intransitive expectations

```bash
$ mulang '
{
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
      ]
   }
}
' | json_pp
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
   ]
}
```

### With unscoped expectations

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Haskell",
      "content" : "x = 1"
   },
   "spec" : {
      "expectations" : [
         {
            "binding" : "*",
            "inspection" : "Declares:x"
         }
      ]
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [
      {
         "result" : true,
         "expectation" : {
            "binding" : "*",
            "inspection" : "Declares:x"
         }
      }
   ]
}
```

### With expectations with matchers

```bash
$ mulang '
{
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
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [
      {
         "expectation" : {
            "inspection" : "Calls:exit:WithNumber:0",
            "binding" : "*"
         },
         "result" : false
      },
      {
         "expectation" : {
            "inspection" : "Prints:WithString:\"bye\"",
            "binding" : "*"
         },
         "result" : true
      }
   ]
}

```


### With custom expectations

> For more information about EDL syntax see [EDL Spec](../edlspec)

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function plusOne(x) { return x + 1 }"
   },
   "spec" : {
      "customExpectations" : "expectation: declares function `plusOne` that (returns with math);\nexpectation: !declares variable with literal"
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [
      {
         "result" : true,
         "expectation" : {
            "binding" : "*",
            "inspection" : "Declares:x"
         }
      }
   ]
}
```

### With signature analysis

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return x + y; }"
   },
   "spec" : {
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "expectationResults" : [
      {
         "expectation" : {
            "binding" : "<<custom>>",
            "inspection" : "E0"
         },
         "result" : true
      },
      {
         "expectation" : {
            "inspection" : "E1",
            "binding" : "<<custom>>"
         },
         "result" : true
      }
   ]
}
```

### With broken input

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y { return x + y; }"
   },
   "spec" : {
      "signatureAnalysisType" : {
        "tag" : "StyledSignatures",
        "style" : "HaskellStyle"
      }
   }
}' | json_pp
{
   "tag" : "AnalysisFailed",
   "reason" : "Sample code parsing error"
}
```

### With AST as input

```bash
$ mulang '
{
   "sample" : {
      "tag" : "MulangSample",
      "ast" : {
         "tag" : "Sequence",
         "contents" : [
            {
              "tag" : "Variable",
              "contents" : [
                "x",
                { "tag" : "MuNumber", "contents" : 1 }
              ]
            },
            {
              "tag" : "Variable",
              "contents" : [
                "y",
                { "tag" : "MuNumber", "contents" : 2 }
              ]
            }
         ]
      }
   },
   "spec" : {
      "signatureAnalysisType" : {
         "tag" : "StyledSignatures",
         "style" : "HaskellStyle"
      }
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "signatures" : [
      "-- x",
      "-- y"
   ]
}
```

### With smell analysis, by inclusion

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "smellsSet" : {
        "tag" : "NoSmells",
        "include" : [
          "ReturnsNil",
          "DoesNullTest"
        ]
      }
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "smells" : [
      {
         "binding" : "foo",
         "inspection" : "ReturnsNil"
      }
   ]
}
```

### With smell analysis, by exclusion

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "smellsSet" : {
        "tag" : "AllSmells",
        "exclude" : [
          "ReturnsNil"
        ]
      }
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "smells" : [],
}
```

### With expressiveness smells

Expressivnes smells are like other smells - they can be included or excluded using the `smellsSet` settings. However, their behaviour is also controlled
by the `domainLanguage` setting, which you _can_ configure:

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "Prolog",
      "content" : "son(Parent, Son):-parentOf(Son, Parent).parentOf(bart, homer)."
   },
   "spec" : {
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : {
         "caseStyle" : "SnakeCase",
         "minimumIdentifierSize" : 4,
         "jargon" : ["id"]
      }
   }
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "smells" : [
      {
         "inspection" : "HasTooShortIdentifiers",
         "binding" : "son"
      },
      {
         "binding" : "parentOf",
         "inspection" : "HasWrongCaseIdentifiers"
      }
   ]
}
```

Also, if you want to use `HasMisspelledIdentifiers` smell, you _need_ to specify a dictionary - with must be ordered, downcased and with unique words only:

```bash
$ mulang  '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "smellsSet" : { "tag" : "AllSmells" },
      "domainLanguage" : { "dictionaryFilePath" : "/usr/share/dict/words" }
   }
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "smells" : [
      {
         "inspection" : "ReturnsNil",
         "binding" : "foo"
      },
      {
         "inspection" : "HasMisspelledIdentifiers",
         "binding" : "foo"
      }
   ]
}
```



### With intermediate language generation

```bash
$ mulang '
{
   "sample" : {
      "tag" : "CodeSample",
      "language" : "JavaScript",
      "content" : "function foo(x, y) { return null; }"
   },
   "spec" : {
      "includeIntermediateLanguage" : true
   }
}
' | json_pp
{
   "tag" : "AnalysisCompleted",
   "intermediateLanguage" : {
      "tag" : "Function",
      "contents" : [
         "foo",
         [
            [
               [
                  {
                     "tag" : "VariablePattern",
                     "contents" : "x"
                  },
                  {
                     "tag" : "VariablePattern",
                     "contents" : "y"
                  }
               ],
               {
                  "tag" : "UnguardedBody",
                  "contents" : {
                     "contents" : {
                        "tag" : "MuNil"
                     },
                     "tag" : "Return"
                  }
               }
            ]
         ]
      ]
   }
}

```


## With test running

```bash
mulang '{
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
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "testResults" : [
      {
         "description" : [
            "f increments by one"
         ],
         "status" : {
            "tag" : "Success"
         }
      }
   ]
}
```
For further detail on this spec, see [Code Execution](#code-execution)

## Code Execution

As of v4.4.0, mulang provides basic support for executing its AST.
This feature can accessed through a `testAnalysisType` spec, such as the one shown in [this section](#with-test-running).

Currently, support is given for executing the following AST elements:

- [Application](#application)
- [Assert](#testgroup-test-and-assert)
- [Assignment](#assignment)
- [ForLoop](#forloop)
- [If](#if)
- [Lambda](#lambda)
- [MuBool](#munumber-mubool-mustring-musymbol-and-muchar)
- [MuList](#mutuple-and-mulist)
- [MuNil](#munil)
- [MuNumber](#munumber-mubool-mustring-musymbol-and-muchar)
- [MuString](#munumber-mubool-mustring-musymbol-and-muchar)
- [Print](#print)
- [Raise](#raise)
- [Reference](#reference)
- [Return](#return)
- [Sequence](#sequence)
- [Function](#function)
- [Procedure](#procedure)
- [Method](#method)
- [Variable](#variable)
- [While](#while)

### Examples

```bash
mulang '{
  "sample" : {
    "tag" : "CodeSample",
    "language" : "JavaScript",
    "content" : "
      function f(x) {
        return x + 1
      }"
  },
  "spec" : {
    "testAnalysisType" : {
      "tag" :  "ExternalTests",
      "test" : {
        "tag" : "CodeSample",
        "language" : "JavaScript",
        "content" : "
          it(\"f increments by one\", function() {
            assert.equals(f(1), 2)
          })"
      }
    }
  }
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "testResults" : [
      {
         "status" : {
            "tag" : "Success"
         },
         "description" : [
            "f increments by one"
         ]
      }
   ]
}
```

Since both the code and tests are parsed to and run as an AST, the two of them needn't be in the same language:

```bash
mulang '{
  "sample" : {
    "tag" : "CodeSample",
    "language" : "Python",
    "content" : "def f():
        x = 0
        while x < 10:
          x += 1
        return x"
  },
  "spec" : {
    "testAnalysisType" : {
      "tag" :  "ExternalTests",
      "test" : {
        "tag" : "CodeSample",
        "language" : "JavaScript",
        "content" : "
          it(\"f returns 10\", function() {
            assert.equals(f(), 10)
          })"
      }
    }
  }
}' | json_pp
{
   "tag" : "AnalysisCompleted",
   "testResults" : [
      {
         "status" : {
            "tag" : "Success"
         },
         "description" : [
            "f returns 10"
         ]
      }
   ]
}

```
