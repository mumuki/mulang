(function() {
  var locale = document.querySelector("html").lang || 'es';

  var keywords = [
    "count",
    "except",
    "in",
    "like",
    "something",
    "expectation",
    "that",
    "through",
    "with",
    "within"
  ];

  const atoms = [
    "anything", "false",  "logic",   "math",  "nil",  "self",  "true", "literal", "nonliteral"
  ];

  const operators = [
    "&&", "and", "\\|\\|", "or", "!", "not", "=", ">=", "<="
  ];

  var buildList = function(values) {
    return values.join('|');
  };

  CodeMirror.defineSimpleMode("edl", {
    start: [
      {regex: /"(?:[^\\]|\\.)*?(?:"|$)/, token: "string"},
      {regex: /`(?:[^\\]|\\.)*?(?:`|$)/, token: "string"},
      {regex: new RegExp(`(?:${buildList(keywords)})\\b`), token: "keyword"},
      {regex: new RegExp(buildList(atoms)), token: "atom"},
      {regex: /[-+]?(\d+)/i, token: "number"},
      {regex: /%%.*/, token: "comment"},
      {regex: /\/(?:[^\\]|\\.)*?\//, token: "variable-3"},
      {regex: new RegExp(buildList(operators)), token: "operator"},
      {regex: /[\:]/, indent: true},
      {regex: /[\;]/, dedent: true},
      {regex: /[a-z]\w*/, token: "variable"}
    ],
    comment: [
      {regex: /.*/, token: "comment"}
    ],
    meta: {
      dontIndentStates: ["comment"],
      lineComment: "%%"
    }
  });

  CodeMirror.defineMIME("text/x-edl", "edl");
})();
