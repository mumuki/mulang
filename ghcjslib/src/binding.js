function ghcjsBinding(functionName) {
  return eval("h$mainZCMainzi" + functionName);
}

function ghcjsApply(fun, arg) {
  return h$c2(h$ap1_e, fun, arg);
}

function ghcjsString(string) {
  return h$c1(h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e, string);
}

function ghcjsWrapper(code) {
  return (functionName) => code(ghcjsBinding(functionName));
}

// Wrappers

// * Any
//    f :: IO (JSVal | JSString)
const ghcjsWrapperUnit = ghcjsWrapper((fun) => () => { h$runSync(fun)});

// * Unit
//    f :: IO ()
const ghcjsWrapperAny = ghcjsWrapper((fun) => () => h$runSyncReturn(fun));

// * ValueUnit
//    f :: (Int | Bool | JSVal) -> IO ()
const ghcjsWrapperValueUnit = ghcjsWrapper((fun) => (arg) => { h$runSync(ghcjsApply(fun, arg)) });

// * ValueAny
//    f :: (Int | Bool | JSVal) -> IO (JSVal | JSString)
const ghcjsWrapperValueAny = ghcjsWrapper((fun) => (arg) => h$runSyncReturn(ghcjsApply(fun, arg)));

// * StringUnit
//    f :: JSString -> IO ()
const ghcjsWrapperStringUnit = ghcjsWrapper((fun) => (arg) => { h$runSync(ghcjsApply(fun, ghcjsString(arg))) });

// * StringAny
//    f :: JSString -> IO (JSVal | JSString)
const ghcjsWrapperStringAny = ghcjsWrapper((fun) => (arg) => h$runSyncReturn(ghcjsApply(fun, ghcjsString(arg))));
