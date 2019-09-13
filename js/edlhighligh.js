hljs.registerLanguage('edl', () => {
  return {
    k: {
      keyword: "count except in like unlike something somewhere expectation that through with within",
      literal: "anything false logic math nil self true literal nonliteral",
      built_in: "and or not"
    },
    c: [
      {cN: 'comment', b: '%', e: '$'},
      {cN: 'string', v: [{b: /"/, e: /"/}]},
      {cN: 'symbol', v: [{b: /`/, e: /`/}]}
    ]
  }
});





