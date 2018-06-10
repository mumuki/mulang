describe("mulang", () => {
  it("can do basic analyis", () => {
    mulang.analyse({
      "sample" : {
         "tag" : "CodeSample",
         "language" : "Haskell",
         "content" : "x = 1"
      },
      "spec" : {
         "expectations" : [
            {
               "binding" : ":Intransitive:x",
               "inspection" : "Uses:*"
            }
         ],
         "smellsSet" : { "tag" : "NoSmells" }
      }
   })
  })

  it("it can generate ast", () => {
    mulang.analyse({
      "sample" : {
          "tag" : "CodeSample",
          "language" : "JavaScript",
          "content" : "function foo(x, y) { return null; }"
      },
      "spec" : {
          "expectations" : [],
          "smellsSet" : { "tag" : "NoSmells" },
          "includeIntermediateLanguage" : true
      }
    })
  })
})
