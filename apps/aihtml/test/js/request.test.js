/* Preserve, settle, request coordination, indicators and the trigger /
   url operations (core.js). fetch() is stubbed with controllable
   responses. */
(function (T, AH) {
  "use strict";

  var pending = [];                  // {body, resolve, aborted}
  window.fetch = function (url, opts) {
    var req = { body: JSON.parse(opts.body), aborted: false };
    pending.push(req);
    return new Promise(function (resolve, reject) {
      req.finish = function () {
        resolve(new Response('data: {"type":"RUN_STARTED"}\n\ndata: {"type":"RUN_FINISHED"}\n\n'));
      };
      opts.signal.addEventListener("abort", function () {
        req.aborted = true;
        var e = new Error("aborted"); e.name = "AbortError"; reject(e);
      });
    });
  };
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  function tick() { return wait(15); }
  var TOK = "AAAA.BBBB";
  function $id(id) { return document.getElementById(id); }
  function click(id) { $id(id).click(); }
  function has(id, cls) { return $id(id).classList.contains(cls); }

  // ---- preserve --------------------------------------------------------

  T.test("preserve keeps the existing node across an inner swap", function (fx) {
    fx.innerHTML = '<div id="box"><input id="keep" data-ah-preserve value="server"><p>a</p></div>';
    var keep = document.getElementById("keep");
    keep.value = "typed";
    AH.swap($id("box"), '<input id="keep" data-ah-preserve value="new"><p>b</p>', "inner");
    T.ok(document.getElementById("keep") === keep, "same node");
    T.eq(keep.value, "typed");
    T.eq($id("box").querySelector("p").textContent, "b");
    T.eq($id("box").children.length, 2);
  });

  T.test("preserve works for outer swaps and morph", function (fx) {
    fx.innerHTML = '<section id="s"><div id="v" data-ah-preserve data-x="1"></div></section>';
    var v = document.getElementById("v");
    AH.swap($id("s"), '<section id="s" class="n"><div id="v" data-ah-preserve data-x="2"></div></section>', "outer");
    T.ok(document.getElementById("v") === v, "kept by outer");
    T.eq(v.getAttribute("data-x"), "1");
    AH.swap($id("s"), '<section id="s"><h1>t</h1><div id="v" data-ah-preserve data-x="3"></div></section>', "morph");
    T.ok(document.getElementById("v") === v, "kept by morph");
    T.eq(v.getAttribute("data-x"), "1");
  });

  // ---- settle ----------------------------------------------------------

  T.test("new content is ah-added until it settles", async function (fx) {
    fx.innerHTML = '<div id="box"></div>';
    AH.swap($id("box"), '<p id="n">x</p>', "inner");
    T.ok(has("n", "ah-added"), "added");
    T.ok(has("box", "ah-settling"), "settling");
    await wait(AH.settleDelay + 20);
    T.ok(!has("n", "ah-added"));
    T.ok(!has("box", "ah-settling"));
  });

  T.test("an element with the same id takes its old class first, then the new one", async function (fx) {
    fx.innerHTML = '<div id="box"><p id="m" class="old" style="width:10px">x</p></div>';
    AH.swap($id("box"), '<p id="m" class="new" style="width:90px">y</p>', "inner");
    var m = document.getElementById("m");
    T.ok(m.classList.contains("old") && !m.classList.contains("new"), "old class right after the swap");
    T.eq(m.style.width, "10px");
    await wait(AH.settleDelay + 20);
    T.eq(m.className, "new");
    T.eq(m.style.width, "90px");
  });

  // ---- sync ------------------------------------------------------------

  T.test("drop (default for click) ignores clicks while a request runs", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-on="click:' + TOK + '">b</button>';
    AH.mount(fx);
    click("b"); click("b"); await tick();
    T.eq(pending.length, 1);
    pending[0].finish(); await tick();
    click("b"); await tick();
    T.eq(pending.length, 2, "a new click after it ended is sent");
    pending[1].finish(); await tick();
  });

  T.test("replace aborts the running request", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-sync="replace" data-ah-on="click:' + TOK + '">b</button>';
    AH.mount(fx);
    click("b"); await tick();
    click("b"); await tick();
    T.eq(pending.length, 2);
    T.ok(pending[0].aborted, "first aborted");
    pending[1].finish(); await tick();
  });

  T.test("queue sends the latest waiting request after the running one", async function (fx) {
    pending = [];
    fx.innerHTML = '<input id="i" data-ah-sync="queue" data-ah-on="change:' + TOK + '">';
    AH.mount(fx);
    var i = document.getElementById("i");
    i.value = "1"; T.fire(i, "change"); await tick();
    i.value = "2"; T.fire(i, "change");
    i.value = "3"; T.fire(i, "change"); await tick();
    T.eq(pending.length, 1, "later ones wait");
    pending[0].finish(); await tick();
    T.eq(pending.length, 2);
    T.eq(pending[1].body.event.value, "3", "only the latest waited");
    pending[1].finish(); await tick();
  });

  T.test("a sync scope makes several elements share one queue", async function (fx) {
    pending = [];
    fx.innerHTML = '<form id="f"><button type="button" id="x" data-ah-sync-scope="form" data-ah-on="click:' + TOK +
      '">x</button><button type="button" id="y" data-ah-sync-scope="form" data-ah-on="click:' + TOK + '">y</button></form>';
    AH.mount(fx);
    click("x"); await tick();
    click("y"); await tick();
    T.eq(pending.length, 1, "y dropped while x runs");
    pending[0].finish(); await tick();
  });

  // ---- indicators ------------------------------------------------------

  T.test("indicators and disabled elements follow the request", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-indicator="#spin" data-ah-disable="#other" data-ah-on="click:' + TOK +
      '">b</button><span id="spin" class="ah-indicator"></span><button id="other">o</button>';
    AH.mount(fx);
    click("b"); await tick();
    T.ok(has("b", "ah-request") && has("spin", "ah-request"), "ah-request on");
    T.eq(document.getElementById("other").disabled, true);
    T.eq($id("b").getAttribute("aria-busy"), "true");
    pending[0].finish(); await tick();
    T.ok(!has("spin", "ah-request"), "ah-request off");
    T.eq(document.getElementById("other").disabled, false);
    T.eq($id("b").getAttribute("aria-busy"), null);
  });

  T.test("a shared indicator stays on until every request ended", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="a" data-ah-indicator="#spin" data-ah-on="click:' + TOK + '">a</button>' +
      '<button id="c" data-ah-indicator="#spin" data-ah-on="click:' + TOK + '">c</button><i id="spin"></i>';
    AH.mount(fx);
    click("a"); click("c"); await tick();
    pending[0].finish(); await tick();
    T.ok(has("spin", "ah-request"), "still on");
    pending[1].finish(); await tick();
    T.ok(!has("spin", "ah-request"), "off");
  });

  T.test("an element that was disabled before stays disabled", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-disable="#o" data-ah-on="click:' + TOK + '">b</button><input id="o" disabled>';
    AH.mount(fx);
    click("b"); await tick();
    pending[0].finish(); await tick();
    T.eq(document.getElementById("o").disabled, true);
  });

  // ---- trigger and url ops ---------------------------------------------

  T.test("trigger ops fire events on the target or the document", function (fx) {
    fx.innerHTML = '<ul id="l"></ul>';
    var got = [];
    var onDoc = function (e) { got.push(["doc", e.detail, e.target === document]); };
    $id("l").addEventListener("refresh", function (e) { got.push(["l", e.detail, e.bubbles]); });
    document.addEventListener("ah:saved", onDoc);
    AH.apply([{ op: "trigger", id: "l", event: "refresh", detail: { n: 1 } },
              { op: "trigger", event: "ah:saved", detail: null }]);
    document.removeEventListener("ah:saved", onDoc);
    T.eq(got, [["l", { n: 1 }, true], ["doc", null, true]]);
  });

  T.test("url ops push and replace history entries", function () {
    var before = history.length;
    AH.apply([{ op: "url", mode: "push", value: "?page=3" }]);
    T.eq(location.search, "?page=3");
    T.eq(history.state, { ah: true });
    T.eq(history.length, before + 1);
    AH.apply([{ op: "url", mode: "replace", value: "?page=4" }]);
    T.eq(location.search, "?page=4");
    T.eq(history.length, before + 1);
  });
  // ---- the other DOM operations -----------------------------------------

  T.test("attr, class, val, focus, remove and title ops", function (fx) {
    fx.innerHTML = '<p id="p" class="a">p</p><input id="t"><input id="c" type="checkbox">' +
      '<select id="s"><option>x</option><option>y</option></select>' +
      '<select id="ms" multiple><option>1</option><option>2</option><option>3</option></select>' +
      '<i class="gone"></i><i class="gone"></i>';
    var title = document.title;
    AH.apply([{ op: "attr", id: "p", name: "data-k", value: "v" },
              { op: "attr", id: "p", name: "hidden", value: "" },
              { op: "class", sel: "#p", add: "b c", remove: "a" },
              { op: "val", id: "t", value: "typed" },
              { op: "val", id: "c", value: true },
              { op: "val", id: "s", value: "y" },
              { op: "val", id: "ms", value: ["1", "3"] },
              { op: "focus", id: "t" },
              { op: "remove", sel: "#fixture .gone" },
              { op: "title", value: "T-title" }]);
    var p = $id("p");
    T.eq([p.getAttribute("data-k"), p.hasAttribute("hidden"), p.className], ["v", true, "b c"]);
    AH.apply([{ op: "attr", id: "p", name: "data-k", value: null },
              { op: "class", id: "p", remove: "b c" }]);
    T.eq([p.hasAttribute("data-k"), p.hasAttribute("class")], [false, false]);
    T.eq($id("t").value, "typed");
    T.eq($id("c").checked, true);
    T.eq($id("s").value, "y");
    T.eq(Array.from($id("ms").selectedOptions).map(function (o) { return o.value; }), ["1", "3"]);
    T.eq(document.activeElement, $id("t"));
    T.eq(fx.querySelectorAll(".gone").length, 0);
    T.eq(document.title, "T-title");
    document.title = title;
  });

  T.test("a failing op is logged and the next ones still run", function (fx) {
    fx.innerHTML = '<p id="p"></p>';
    var err = console.error, logged = 0;
    console.error = function () { logged++; };
    AH.apply([{ op: "attr", sel: "::bad(", name: "x", value: "1" },
              { op: "attr", id: "p", name: "x", value: "1" }]);
    console.error = err;
    T.eq(logged, 1);
    T.eq($id("p").getAttribute("x"), "1");
  });

  T.test("RUN_ERROR and refused actions fire ah:error on the element", async function (fx) {
    var real = window.fetch, err = console.error;
    var replies = ['data: {"type":"RUN_ERROR","message":"boom","code":"x"}\n\n'];
    window.fetch = function () {
      var body = replies.shift();
      return Promise.resolve(body ? new Response(body) : new Response("no", { status: 403 }));
    };
    console.error = function () {};
    fx.innerHTML = '<button id="b" data-ah-on="click:' + TOK + '">b</button>';
    AH.mount(fx);
    var got = [];
    $id("b").addEventListener("ah:error", function (e) { got.push(e.detail); });
    click("b"); await tick();
    click("b"); await tick();
    window.fetch = real;
    console.error = err;
    T.eq(got, [{ message: "boom", code: "x" }, { status: 403 }]);
  });
})(window.AHTest, window.AH);
