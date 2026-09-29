/* Preserve, settle, request coordination, indicators and the trigger /
   url operations (core.js). fetch() is stubbed with controllable
   responses. */
(function (T, $, AH) {
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

  // ---- preserve --------------------------------------------------------

  T.test("preserve keeps the existing node across an inner swap", function (fx) {
    fx.innerHTML = '<div id="box"><input id="keep" data-ah-preserve value="server"><p>a</p></div>';
    var keep = document.getElementById("keep");
    keep.value = "typed";
    AH.swap($("#box"), '<input id="keep" data-ah-preserve value="new"><p>b</p>', "inner");
    T.ok(document.getElementById("keep") === keep, "same node");
    T.eq(keep.value, "typed");
    T.eq($("#box p").text(), "b");
    T.eq($("#box").children().length, 2);
  });

  T.test("preserve works for outer swaps and morph", function (fx) {
    fx.innerHTML = '<section id="s"><div id="v" data-ah-preserve data-x="1"></div></section>';
    var v = document.getElementById("v");
    AH.swap($("#s"), '<section id="s" class="n"><div id="v" data-ah-preserve data-x="2"></div></section>', "outer");
    T.ok(document.getElementById("v") === v, "kept by outer");
    T.eq(v.getAttribute("data-x"), "1");
    AH.swap($("#s"), '<section id="s"><h1>t</h1><div id="v" data-ah-preserve data-x="3"></div></section>', "morph");
    T.ok(document.getElementById("v") === v, "kept by morph");
    T.eq(v.getAttribute("data-x"), "1");
  });

  // ---- settle ----------------------------------------------------------

  T.test("new content is ah-added until it settles", async function (fx) {
    fx.innerHTML = '<div id="box"></div>';
    AH.swap($("#box"), '<p id="n">x</p>', "inner");
    T.ok($("#n").hasClass("ah-added"), "added");
    T.ok($("#box").hasClass("ah-settling"), "settling");
    await wait(AH.settleDelay + 20);
    T.ok(!$("#n").hasClass("ah-added"));
    T.ok(!$("#box").hasClass("ah-settling"));
  });

  T.test("an element with the same id takes its old class first, then the new one", async function (fx) {
    fx.innerHTML = '<div id="box"><p id="m" class="old" style="width:10px">x</p></div>';
    AH.swap($("#box"), '<p id="m" class="new" style="width:90px">y</p>', "inner");
    var m = document.getElementById("m");
    T.ok($(m).hasClass("old") && !$(m).hasClass("new"), "old class right after the swap");
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
    $("#b").trigger("click"); $("#b").trigger("click"); await tick();
    T.eq(pending.length, 1);
    pending[0].finish(); await tick();
    $("#b").trigger("click"); await tick();
    T.eq(pending.length, 2, "a new click after it ended is sent");
    pending[1].finish(); await tick();
  });

  T.test("replace aborts the running request", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-sync="replace" data-ah-on="click:' + TOK + '">b</button>';
    AH.mount(fx);
    $("#b").trigger("click"); await tick();
    $("#b").trigger("click"); await tick();
    T.eq(pending.length, 2);
    T.ok(pending[0].aborted, "first aborted");
    pending[1].finish(); await tick();
  });

  T.test("queue sends the latest waiting request after the running one", async function (fx) {
    pending = [];
    fx.innerHTML = '<input id="i" data-ah-sync="queue" data-ah-on="change:' + TOK + '">';
    AH.mount(fx);
    var i = document.getElementById("i");
    i.value = "1"; $(i).trigger("change"); await tick();
    i.value = "2"; $(i).trigger("change");
    i.value = "3"; $(i).trigger("change"); await tick();
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
    $("#x").trigger("click"); await tick();
    $("#y").trigger("click"); await tick();
    T.eq(pending.length, 1, "y dropped while x runs");
    pending[0].finish(); await tick();
  });

  // ---- indicators ------------------------------------------------------

  T.test("indicators and disabled elements follow the request", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-indicator="#spin" data-ah-disable="#other" data-ah-on="click:' + TOK +
      '">b</button><span id="spin" class="ah-indicator"></span><button id="other">o</button>';
    AH.mount(fx);
    $("#b").trigger("click"); await tick();
    T.ok($("#b").hasClass("ah-request") && $("#spin").hasClass("ah-request"), "ah-request on");
    T.eq(document.getElementById("other").disabled, true);
    T.eq($("#b").attr("aria-busy"), "true");
    pending[0].finish(); await tick();
    T.ok(!$("#spin").hasClass("ah-request"), "ah-request off");
    T.eq(document.getElementById("other").disabled, false);
    T.eq($("#b").attr("aria-busy"), undefined);
  });

  T.test("a shared indicator stays on until every request ended", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="a" data-ah-indicator="#spin" data-ah-on="click:' + TOK + '">a</button>' +
      '<button id="c" data-ah-indicator="#spin" data-ah-on="click:' + TOK + '">c</button><i id="spin"></i>';
    AH.mount(fx);
    $("#a").trigger("click"); $("#c").trigger("click"); await tick();
    pending[0].finish(); await tick();
    T.ok($("#spin").hasClass("ah-request"), "still on");
    pending[1].finish(); await tick();
    T.ok(!$("#spin").hasClass("ah-request"), "off");
  });

  T.test("an element that was disabled before stays disabled", async function (fx) {
    pending = [];
    fx.innerHTML = '<button id="b" data-ah-disable="#o" data-ah-on="click:' + TOK + '">b</button><input id="o" disabled>';
    AH.mount(fx);
    $("#b").trigger("click"); await tick();
    pending[0].finish(); await tick();
    T.eq(document.getElementById("o").disabled, true);
  });

  // ---- trigger and url ops ---------------------------------------------

  T.test("trigger ops fire events on the target or the document", function (fx) {
    fx.innerHTML = '<ul id="l"></ul>';
    var got = [];
    $("#l").on("refresh", function (e, d) { got.push(["l", d]); });
    $(document).on("ah:saved.t", function (e, d) { got.push(["doc", d]); });
    AH.apply([{ op: "trigger", id: "l", event: "refresh", detail: { n: 1 } },
              { op: "trigger", event: "ah:saved", detail: null }]);
    $(document).off(".t");
    T.eq(got, [["l", { n: 1 }], ["doc", null]]);
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
})(window.AHTest, window.jQuery, window.AH);
