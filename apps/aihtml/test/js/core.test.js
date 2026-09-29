/* Runtime behaviour shared by all components (core.js). fetch() is stubbed:
   each test sees the action requests the page would send, and the
   data-ah-fetch round trips (answered by `pages`). */
(function (T, AH) {
  "use strict";

  var sent = [];
  var fetched = [];                  // data-ah-fetch requests: {url, init}
  var pages = {};                    // url path -> {status, body}
  window.fetch = function (url, opts) {
    if (opts.headers && opts.headers["X-Aihtml"]) {
      fetched.push({ url: url, init: opts });
      var p = pages[url.replace(/\?.*$/, "")] || { status: 404, body: "missing" };
      return Promise.resolve(new Response(p.body, { status: p.status }));
    }
    sent.push(JSON.parse(opts.body));
    var body = '{"ops":[]}';
    return Promise.resolve(new Response(body, { status: 200 }));
  };
  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }
  var TOKEN = "AAAA.BBBB";

  T.test("data-ah-value wins over a native value", async function (fx) {
    sent = [];
    fx.innerHTML = '<button id="tb" value="native" data-ah-value="true" data-ah-on="click:' + TOKEN + '">x</button>';
    AH.mount(fx);
    document.getElementById("tb").click();
    await wait(20);
    T.eq(sent.length, 1);
    T.eq(sent[0].event.value, "true");
  });

  T.test("a change bubbling from inside a value-bearing root is not its change", async function (fx) {
    sent = [];
    fx.innerHTML = '<div id="root" data-ah-value="a" data-ah-on="change:' + TOKEN + '"><input id="inner"></div>';
    AH.mount(fx);
    T.fire(document.getElementById("inner"), "change");
    await wait(20);
    T.eq(sent.length, 0, "bubbled change ignored");
    T.fire(document.getElementById("root"), "change");
    await wait(20);
    T.eq(sent.length, 1, "the root's own change is sent");
    T.eq(sent[0].event.value, "a");
  });

  T.test("component events (ah:close) can carry actions", async function (fx) {
    sent = [];
    fx.innerHTML = '<div id="tabs" data-ah-on="ah:close:' + TOKEN + '"></div>';
    AH.mount(fx);                    // registers a listener for ah:close
    T.fire(document.getElementById("tabs"), "ah:close");
    await wait(20);
    T.eq(sent.length, 1);
    T.eq(sent[0].action, TOKEN);
    T.eq(sent[0].event.type, "ah:close");
  });

  T.test("the event payload carries the form, included values and data", async function (fx) {
    sent = [];
    fx.innerHTML = '<form><input name="q" value="x y"><input type="checkbox" name="c" value="on">' +
      '<select name="m" multiple><option selected>a</option><option>b</option><option selected>c</option></select>' +
      '<input name="off" value="1" disabled>' +
      '<button type="button" id="go" data-row="7" data-ah-include="#extra" data-ah-on="click:' + TOKEN + '">go</button>' +
      '</form><input id="extra" value="e">';
    AH.mount(fx);
    document.getElementById("go").click();
    await wait(20);
    T.eq(sent[0].event.form, { q: "x y", m: "c" });
    T.eq(sent[0].event.values, { extra: "e" });
    T.eq(sent[0].event.data, { row: "7" });
    T.eq(sent[0].event.id, "go");
  });

  T.test("mouseenter actions fire on the element, not for its children", async function (fx) {
    sent = [];
    fx.innerHTML = '<div id="h" data-ah-on="mouseenter:' + TOKEN + '"><span id="hc">x</span></div>';
    AH.mount(fx);
    T.fire(document.getElementById("h"), "mouseover", { relatedTarget: document.body });
    T.fire(document.getElementById("h"), "mouseenter", { bubbles: false });
    await wait(20);
    T.eq(sent.length, 1);
    T.eq(sent[0].event.type, "mouseenter");
  });

  T.test("call ops run behaviour methods and page functions", async function (fx) {
    var calls = [];
    AH.register("t-call", class extends AH.Controller {
      open(a, b) { calls.push([this.element.id, a, b]); }
    });
    AH.fn("t-global", function (x) { calls.push(["global", x]); });
    fx.innerHTML = '<div id="w" data-ah="t-call"></div>';
    AH.mount(fx);
    await T.ready(fx);
    AH.apply([{ op: "call", id: "w", method: "open", args: [1, "two"] },
              { op: "call", method: "t-global", args: [{ k: 3 }] }]);
    T.eq(calls, [["w", 1, "two"], ["global", { k: 3 }]]);
  });

  T.test("a call op before the controller connected runs once it has", async function (fx) {
    var calls = [];
    AH.register("t-late", class extends AH.Controller {
      ping(x) { calls.push(x); }
    });
    AH.apply([{ op: "html", id: "fixture", html: '<div id="late" data-ah="t-late"></div>' },
              { op: "call", id: "late", method: "ping", args: ["now"] }]);
    await T.ready(fx);
    T.eq(calls, ["now"]);
  });

  T.test("AH.invoke takes an element, a selector or a jQuery-like object", async function (fx) {
    var seen = [];
    AH.register("t-inv", class extends AH.Controller {
      who() { seen.push(this.element.id); return this.element.id; }
    });
    fx.innerHTML = '<i id="i1" data-ah="t-inv"></i><i id="i2" data-ah="t-inv"></i>';
    await T.ready(fx);
    T.eq(AH.invoke(document.getElementById("i1"), "who"), "i1");
    AH.invoke("#fixture i", "who");
    AH.invoke({ jquery: "x", length: 1, 0: document.getElementById("i2") }, "who");
    T.eq(seen, ["i1", "i1", "i2", "i2"]);
  });

  T.test("destroy runs teardown, mount runs setup again", async function (fx) {
    var log = [];
    AH.register("t-life", class extends AH.Controller {
      setup() {
        log.push("setup");
        this.listen(this.element, "ping", function () { log.push("ping"); });
      }
      teardown() { log.push("teardown"); }
    });
    fx.innerHTML = '<div id="l" data-ah="t-life"></div>';
    await T.ready(fx);
    var l = document.getElementById("l");
    AH.destroy(l);
    T.fire(l, "ping");
    AH.mount(fx);
    T.fire(l, "ping");
    T.eq(log, ["setup", "teardown", "setup", "ping"]);
    l.remove();
    await wait(0);
    T.eq(log.length, 5, "teardown on removal");
    fx.appendChild(l);
    await T.ready(fx);
    T.eq(log[5], "setup", "set up again when re-inserted");
  });

  T.test("shared templates are compiled into AH.tpl", function () {
    var html = AH.tpl.tag_input_chip({ variant: "soft", color: "primary", index: 2,
                                        label: "<x>", disabled: true });
    T.ok(html.indexOf("&lt;x&gt;") > 0, "escaped");
    T.ok(/ disabled>/.test(html), "section rendered");
    T.ok(!/\n$/.test(html), "no trailing newline");
  });

  // ---- data-ah-fetch round trips (fetch) --------------------------------

  T.test("fetch: a form GETs its fields and swaps the response in", async function (fx) {
    fetched = [];
    pages["/t/search"] = { status: 200, body: '<p id="res">found</p>' };
    fx.innerHTML = '<form id="f" data-ah-fetch="get" data-ah-url="/t/search" data-ah-target="#out">' +
      '<input name="q" value="a b"><input type="checkbox" name="x" value="1" checked>' +
      '<input type="checkbox" name="y" value="1"></form><div id="out">old</div>';
    var events = [];
    fx.addEventListener("ah:before-fetch", function (e) { events.push(["before", e.detail]); });
    fx.addEventListener("ah:after-fetch", function (e) { events.push(["after", e.detail]); });
    T.fire(document.getElementById("f"), "submit");
    await wait(20);
    T.eq(fetched.length, 1);
    T.eq(fetched[0].url, "/t/search?q=a+b&x=1");
    T.eq(fetched[0].init.method, "GET");
    T.eq(fetched[0].init.headers["X-Aihtml"], "1");
    T.eq(fetched[0].init.headers["X-Aihtml-Target"], "#out");
    T.eq(document.getElementById("res").parentNode.id, "out");
    T.eq(document.getElementById("out").textContent, "found");
    T.eq(events, [["before", { url: "/t/search", method: "GET" }], ["after", { url: "/t/search" }]]);
    T.eq(document.getElementById("f").getAttribute("aria-busy"), null, "request state cleared");
  });

  T.test("fetch: POST sends a form-encoded body, outer swap mounts the new content", async function (fx) {
    fetched = [];
    var sets = 0;
    AH.register("t-fetched", class extends AH.Controller { setup() { sets++; } });
    pages["/t/save"] = { status: 200, body: '<div id="card" data-ah="t-fetched">saved</div>' };
    fx.innerHTML = '<div id="card"><input id="name" name="name" value="Zoë" data-ah-fetch="post" ' +
      'data-ah-url="/t/save" data-ah-target="#card" data-ah-swap="outer"></div>';
    T.fire(document.getElementById("name"), "change");
    await wait(20);
    T.eq(fetched[0].init.method, "POST");
    T.eq(fetched[0].init.body, "name=Zo%C3%AB");
    T.ok(/x-www-form-urlencoded/.test(fetched[0].init.headers["Content-Type"]));
    T.eq(document.getElementById("card").textContent, "saved");
    await T.ready(fx);
    T.eq(sets, 1);
  });

  T.test("fetch: before-fetch can cancel, errors fire ah:error", async function (fx) {
    fetched = [];
    fx.innerHTML = '<button id="b" data-ah-fetch="get" data-ah-url="/t/none">b</button>';
    var b = document.getElementById("b");
    var stop = function (e) { e.preventDefault(); };
    b.addEventListener("ah:before-fetch", stop);
    b.click();
    await wait(20);
    T.eq(fetched.length, 0, "cancelled");
    b.removeEventListener("ah:before-fetch", stop);
    var err = null;
    b.addEventListener("ah:error", function (e) { err = e.detail; });
    var ok = await AH.fetch(b);
    T.eq(ok, false);
    T.eq(err, { url: "/t/none", status: 404, body: "missing" });
    T.eq(b.textContent, "b", "nothing swapped");
    T.ok(!b.classList.contains("ah-request"), "request state cleared");
  });

  // ---- theme ------------------------------------------------------------

  T.test("theme.set updates <html>, stores the choice and fires ah:theme", function () {
    var html = document.documentElement, before = html.getAttribute("data-palette");
    var got = null;
    var on = function (e) { got = e.detail; };
    document.addEventListener("ah:theme", on);
    AH.theme.set("palette", "t-pal");
    document.removeEventListener("ah:theme", on);
    T.eq(html.getAttribute("data-palette"), "t-pal");
    T.eq(AH.theme.get().palette, "t-pal");
    T.eq(JSON.parse(localStorage.getItem("aihtml.theme")).palette, "t-pal");
    T.eq(got, { axis: "palette", value: "t-pal" });
    var threw = false;
    try { AH.theme.set("nope", "x"); } catch (e) { threw = true; }
    T.ok(threw, "unknown axis");
    if (before === null) { html.removeAttribute("data-palette"); } else { html.setAttribute("data-palette", before); }
    AH.theme.reset();
  });

  T.test("theme switcher: follows <html> and sets the theme on change", async function (fx) {
    var html = document.documentElement, before = html.getAttribute("data-theme");
    html.setAttribute("data-theme", "dark");
    fx.innerHTML = '<div id="ts" data-ah="theme-switcher"><select id="ax" data-ah-axis="appearance">' +
      '<option value="light">light</option><option value="dark">dark</option></select></div>';
    await T.ready(fx);
    var ax = document.getElementById("ax");
    T.eq(ax.value, "dark", "synced on setup");
    ax.value = "light";
    T.fire(ax, "change");
    T.eq(html.getAttribute("data-theme"), "light");
    AH.theme.set("appearance", "dark");
    T.eq(ax.value, "dark", "follows AH.theme.set");
    // cleanup: removed and re-inserted, it still works
    var ts = document.getElementById("ts");
    ts.remove();
    await wait(0);
    AH.theme.set("appearance", "light");
    T.eq(ax.value, "dark", "no longer listening once removed");
    fx.appendChild(ts);
    await T.ready(fx);
    T.eq(ax.value, "light", "synced again when re-inserted");
    if (before === null) { html.removeAttribute("data-theme"); } else { html.setAttribute("data-theme", before); }
    AH.theme.reset();
  });
})(window.AHTest, window.AH);
