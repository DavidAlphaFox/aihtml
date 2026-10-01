/* data-ah-on-client (aihtml:on_client/2): operations the browser applies itself when
   an event fires, without a request (runtime/actions.ts). */
(function (T, AH) {
  "use strict";

  function $id(id) { return document.getElementById(id); }
  function attr(ops) { return JSON.stringify(ops).replace(/"/g, "&quot;"); }
  async function html(fx, s) { fx.innerHTML = s; AH.mount(fx); await T.ready(fx); }

  T.test("on_client: a click applies its operations in order", async function (fx) {
    await html(fx, '<button id="b" data-ah-on-client="' + attr({ click: [
      { op: "class", id: "t", add: "on" },
      { op: "attr", id: "t", name: "data-n", value: "1" },
      { op: "class", id: "t", remove: "off" }] }) + '">x</button><p id="t" class="off"></p>');
    $id("b").click();
    T.eq($id("t").getAttribute("class"), "on");
    T.eq($id("t").getAttribute("data-n"), "1");
  });

  T.test("on_client: call reaches a component method, also by selector", async function (fx) {
    var got = [];
    AH.register("t-on-client-call", class extends AH.Controller {
      hit(a, b) { got.push(this.element.id + ":" + a + b); }
    });
    await html(fx, '<div data-ah="t-on-client-call" id="c1" class="c"></div><div data-ah="t-on-client-call" id="c2" class="c"></div>' +
               '<button id="b" data-ah-on-client="' + attr({ click: [{ op: "call", sel: ".c", method: "hit", args: [1, "x"] }] }) +
               '">x</button>');
    $id("b").click();
    T.eq(got, ["c1:1x", "c2:1x"]);
  });

  T.test("on_client: other events only on their own; the element's other events do nothing", async function (fx) {
    await html(fx, '<input id="i" data-ah-on-client="' + attr({ change: [{ op: "class", id: "t", add: "changed" }] }) +
               '"><p id="t"></p>');
    T.fire($id("i"), "input");
    T.ok(!$id("t").classList.contains("changed"), "input does not run change");
    T.fire($id("i"), "change");
    T.ok($id("t").classList.contains("changed"));
  });

  T.test("on_client: a link or submit button is not followed", async function (fx) {
    await html(fx, '<form id="f"><a id="a" href="#nowhere" data-ah-on-client="' + attr({ click: [{ op: "class", id: "t", add: "a" }] }) +
               '">a</a><button id="s" type="submit" data-ah-on-client="' + attr({ click: [{ op: "class", id: "t", add: "s" }] }) +
               '">s</button></form><p id="t"></p>');
    var submitted = 0;
    $id("f").addEventListener("submit", function (e) { submitted++; e.preventDefault(); });
    var before = location.hash;
    $id("a").click();
    $id("s").click();
    T.eq(location.hash, before);
    T.eq(submitted, 0);
    T.eq($id("t").getAttribute("class"), "a s");
  });

  T.test("on_client: component events bound by inserted content are listened for", async function (fx) {
    await html(fx, '<div id="box"></div><p id="t"></p>');
    AH.swap($id("box"), '<span id="s" data-ah-on-client="' +
            attr({ "ah:ping": [{ op: "attr", id: "t", name: "data-ping", value: "yes" }] }) + '"></span>', "inner");
    AH.mount($id("box"));
    $id("s").dispatchEvent(new CustomEvent("ah:ping", { bubbles: true }));
    T.eq($id("t").getAttribute("data-ping"), "yes");
  });

  T.test("on_client: runs before the element's data-ah-on action", async function (fx) {
    var fetch = window.fetch, order = [];
    window.fetch = function () {
      order.push("post:" + $id("t").className);
      return Promise.resolve(new Response('{"ops":[]}', { headers: { "Content-Type": "application/json" } }));
    };
    try {
      await html(fx, '<button id="b" data-ah-on="click:AAAA.BBBB" data-ah-on-client="' +
                 attr({ click: [{ op: "class", id: "t", add: "busy" }] }) + '">x</button><p id="t"></p>');
      $id("b").click();
      await new Promise(function (r) { setTimeout(r, 10); });
      T.eq(order, ["post:busy"]);
    } finally {
      window.fetch = fetch;
    }
  });

  T.test("on_client: an unreadable attribute is logged, not thrown", async function (fx) {
    var err = console.error, logged = 0;
    console.error = function () { logged++; };
    try {
      await html(fx, '<button id="b" data-ah-on-client="{not json">x</button>');
      $id("b").click();
    } finally {
      console.error = err;
    }
    T.ok(logged >= 1, "logged");
  });

  T.test("on_client: Stimulus actions are not read from data-ah-on-client", async function (fx) {
    var hits = 0;
    AH.register("t-on-client-stim", class extends AH.Controller { go() { hits++; } });
    await html(fx, '<div data-ah="t-on-client-stim"><button id="b" data-ah-on-client="click->t-on-client-stim#go">x</button></div>');
    var err = console.error;
    console.error = function () {};
    try { $id("b").click(); } finally { console.error = err; }
    T.eq(hits, 0);
  });
})(window.AHTest, window.AH);
