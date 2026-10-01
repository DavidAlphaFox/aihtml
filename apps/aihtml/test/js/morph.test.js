/* Morph swapping and focus preservation (core.js swap/morph). */
(function (T, AH) {
  "use strict";

  async function html(fx, s) { fx.innerHTML = s; AH.mount(fx); await T.ready(fx); }
  function box() { return document.getElementById("box"); }

  T.test("morph keeps the focused input, its caret and what was typed", async function (fx) {
    await html(fx, '<div id="box"><p class="msg">old</p><input id="q" value="server"></div>');
    var q = document.getElementById("q");
    q.focus();
    q.value = "typed by user";
    q.setSelectionRange(3, 5);
    AH.swap(box(), '<div id="box"><p class="msg new">new</p><input id="q" value="server2"></div>', "morph");
    T.ok(document.getElementById("q") === q, "same input node");
    T.eq(document.activeElement, q);
    T.eq(q.value, "typed by user");
    T.eq([q.selectionStart, q.selectionEnd], [3, 5]);
    T.eq(box().querySelector(".msg").textContent, "new");
    T.eq(box().querySelector(".msg").getAttribute("class"), "msg new");
  });

  T.test("morph updates an unfocused input's value from the server", async function (fx) {
    await html(fx, '<div id="box"><input id="a" value="1"><input id="b" type="checkbox"></div>');
    AH.swap(box(), '<div id="box"><input id="a" value="2"><input id="b" type="checkbox" checked></div>', "morph");
    T.eq(document.getElementById("a").value, "2");
    T.eq(document.getElementById("b").checked, true);
  });

  T.test("morph matches by id, so reordered nodes keep their identity", async function (fx) {
    await html(fx, '<ul id="l"><li id="x">x</li><li id="y">y</li><li id="z">z</li></ul>');
    var y = document.getElementById("y");
    AH.swap(document.getElementById("l"), '<ul id="l"><li id="y">Y</li><li id="z">z</li><li id="w">w</li></ul>', "morph");
    T.ok(document.getElementById("y") === y, "y kept");
    T.eq(Array.from(document.querySelectorAll("#l li")).map(function (li) { return li.id + ":" + li.textContent; }),
         ["y:Y", "z:z", "w:w"]);
  });

  T.test("morph_inner patches the children and drops removed ones", async function (fx) {
    await html(fx, '<div id="box"><span>a</span><b>b</b><i>c</i></div>');
    var span = fx.querySelector("span");
    AH.swap(box(), "<span>A</span><em>e</em>", "morph_inner");
    T.ok(fx.querySelector("span") === span, "span kept");
    T.ok(fx.querySelector("em").classList.contains("ah-added"), "new node is settling");
    await new Promise(function (r) { setTimeout(r, AH.settleDelay + 20); });
    T.eq(box().innerHTML, "<span>A</span><em>e</em>");
  });

  T.test("morph re-initialises a changed component and mounts added ones", async function (fx) {
    var inits = [], destroys = [];
    AH.register("t-morph", class extends AH.Controller {
      setup() { inits.push(this.element.getAttribute("data-v")); }
      teardown() { destroys.push(this.element.getAttribute("data-v")); }
      live() { return !!this.signal && !this.signal.aborted; }
    });
    await html(fx, '<div id="box"><div data-ah="t-morph" id="c1" data-v="1"></div></div>');
    T.eq(inits, ["1"]);
    var c1 = document.getElementById("c1");
    AH.swap(box(), '<div id="box"><div data-ah="t-morph" id="c1" data-v="2"></div>' +
                   '<div data-ah="t-morph" id="c2" data-v="new"></div></div>', "morph");
    await T.ready(fx);
    T.ok(document.getElementById("c1") === c1, "component node kept");
    T.eq(destroys, ["2"]);          // destroyed after its attributes were synced
    T.eq(inits, ["1", "2", "new"]);
    T.ok(AH.invoke(c1, "live"), "still mounted");
  });

  T.test("an unchanged component is left alone", async function (fx) {
    var inits = 0;
    AH.register("t-still", class extends AH.Controller { setup() { inits++; } });
    await html(fx, '<div id="box"><div data-ah="t-still" id="s"></div><p>1</p></div>');
    AH.swap(box(), '<div id="box"><div data-ah="t-still" id="s"></div><p>2</p></div>', "morph");
    await T.ready(fx);
    T.eq(inits, 1);
  });

  T.test("a component moved by morph keeps its controller (no teardown/setup)", async function (fx) {
    var log = [];
    AH.register("t-moved", class extends AH.Controller {
      setup() { log.push("setup " + this.element.id); }
      teardown() { log.push("teardown " + this.element.id); }
    });
    await html(fx, '<div id="box"><p id="p1">a</p><div id="m" data-ah="t-moved"></div></div>');
    var m = document.getElementById("m");
    AH.swap(box(), '<div id="box"><div id="m" data-ah="t-moved"></div><p id="p1">a</p></div>', "morph");
    await new Promise(function (r) { setTimeout(r, 10); });
    await T.ready(fx);
    T.ok(document.getElementById("m") === m, "same node");
    T.eq(box().firstElementChild, m);
    T.eq(log, ["setup m"]);
  });

  T.test("a plain inner swap puts the focus back by id", async function (fx) {
    await html(fx, '<div id="box"><input id="f" value="hello"></div>');
    var f = document.getElementById("f");
    f.focus();
    f.setSelectionRange(1, 4);
    AH.swap(box(), '<input id="f" value="hello">', "inner");
    var g = document.getElementById("f");
    T.ok(g !== f, "node was replaced");
    T.eq(document.activeElement, g);
    T.eq([g.selectionStart, g.selectionEnd], [1, 4]);
  });

  T.test("swap returns the inserted nodes, drops scripts, takes several targets", async function (fx) {
    await html(fx, '<div class="t" id="t1"></div><div class="t" id="t2"></div>');
    window.ahSwapRan = false;
    var added = AH.swap("#fixture .t", '<b>x</b><script>window.ahSwapRan = true;</script>' +
                        '<script type="application/json">{"a":1}</script>', "append");
    T.eq(added.map(function (n) { return n.nodeName; }), ["B", "SCRIPT", "B", "SCRIPT"]);
    T.eq(document.getElementById("t1").innerHTML, '<b class="ah-added">x</b><script type="application/json" class="ah-added">{"a":1}</script>');
    T.eq(document.getElementById("t2").querySelectorAll("b").length, 1);
    T.eq(window.ahSwapRan, false, "scripts are not run");
    T.ok(document.getElementById("t1").classList.contains("ah-settling"));
    T.eq(AH.swap(document.getElementById("t1"), "<i>y</i>", "none"), []);
  });

  T.test("morph (outer) needs a single root", async function (fx) {
    await html(fx, '<div id="box"></div>');
    var threw = false;
    try { AH.swap(box(), "<p>a</p><p>b</p>", "morph"); } catch (e) { threw = true; }
    T.ok(threw);
  });

  T.test("server ops can morph", async function (fx) {
    await html(fx, '<div id="box"><input id="k" value="v"></div>');
    var k = document.getElementById("k");
    k.focus();
    AH.apply([{ op: "html", id: "box", swap: "morph", html: '<div id="box" class="done"><input id="k" value="v"></div>' }]);
    T.ok(document.getElementById("k") === k);
    T.eq(document.activeElement, k);
    await new Promise(function (r) { setTimeout(r, AH.settleDelay + 20); });
    T.eq(box().className, "done");
  });

  T.test("AH.morph patches an element in place", async function (fx) {
    await html(fx, '<div id="box"><span id="s">1</span></div>');
    var s = document.getElementById("s");
    AH.morph(box(), '<div id="box" data-x="y"><span id="s">2</span></div>');
    T.ok(document.getElementById("s") === s);
    T.eq(s.textContent, "2");
    T.eq(box().getAttribute("data-x"), "y");
  });
  // ---- attrs: typed values that follow data-ah-* attributes ----------

  function settle() { return new Promise(function (r) { setTimeout(r, 0); }); }

  T.test("attrs: typed accessors, defaults, and the setter writes the attribute", async function (fx) {
    AH.register("t-attrs", class extends AH.Controller {
      static attrs = { count: Number, label: { type: String, default: "none" }, open: Boolean,
                       maxDepth: Number, opts: Object };
      read() { return [this.countValue, this.labelValue, this.openValue, this.maxDepthValue, this.optsValue]; }
      write() { this.countValue = 5; this.optsValue = { a: 1 }; this.labelValue = null; }
    });
    await html(fx, '<div id="a" data-ah="t-attrs" data-ah-count="3" data-ah-open="false"' +
                   ' data-ah-max-depth="2" data-ah-opts=\'{"x":true}\'></div>');
    var a = document.getElementById("a");
    T.eq(AH.invoke(a, "read"), [3, "none", false, 2, { x: true }]);
    AH.invoke(a, "write");
    T.eq(a.getAttribute("data-ah-count"), "5");
    T.eq(a.getAttribute("data-ah-opts"), '{"a":1}');
    T.ok(!a.hasAttribute("data-ah-label"), "null removes");
  });

  T.test("attrs: <key>ValueChanged runs for changes after setup, not for the initial value", async function (fx) {
    var log = [];
    AH.register("t-changed", class extends AH.Controller {
      static attrs = { count: Number };
      setup() { log.push("setup"); this.element.setAttribute("data-ah-count", "1"); }
      countValueChanged(v, old) { log.push(old + "->" + v); }
    });
    await html(fx, '<div id="c" data-ah="t-changed" data-ah-count="0"></div>');
    await settle();
    T.eq(log, ["setup"]);
    var c = document.getElementById("c");
    c.setAttribute("data-ah-count", "2");
    c.setAttribute("data-ah-count", "4");      // one report for both
    await settle();
    T.eq(log, ["setup", "1->4"]);
    c.setAttribute("data-ah-count", "4");
    await settle();
    T.eq(log.length, 2);
    AH.destroy(c);
    c.setAttribute("data-ah-count", "9");
    await settle();
    T.eq(log.length, 2, "nothing after teardown");
  });

  T.test("morph keeps a component that declares attrs set up and reports its values", async function (fx) {
    var log = [];
    AH.register("t-follow", class extends AH.Controller {
      static attrs = { value: String };
      setup() { log.push("setup"); }
      teardown() { log.push("teardown"); }
      valueValueChanged(v, old) { log.push(old + "->" + v); }
    });
    await html(fx, '<div id="box"><div id="f" data-ah="t-follow" data-ah-value="a"><p>a</p></div></div>');
    AH.swap(box(), '<div id="box"><div id="f" data-ah="t-follow" data-ah-value="b"><p>b</p><p>more</p></div></div>', "morph");
    await settle();
    await T.ready(fx);
    T.eq(log, ["setup", "a->b"]);
    T.eq(document.querySelectorAll("#f p").length, 2);
  });

  T.test("morph re-initialises a component with attrs that becomes another behaviour", async function (fx) {
    var log = [];
    AH.register("t-follow-a", class extends AH.Controller {
      static attrs = { value: String };
      setup() { log.push("setup a"); }
      teardown() { log.push("teardown a"); }
    });
    AH.register("t-follow-b", class extends AH.Controller { setup() { log.push("setup b"); } });
    await html(fx, '<div id="box"><div id="f" data-ah="t-follow-a" data-ah-value="1"></div></div>');
    AH.swap(box(), '<div id="box"><div id="f" data-ah="t-follow-b" data-ah-value="1"></div></div>', "morph");
    await settle();
    await T.ready(fx);
    T.eq(log.slice().sort(), ["setup a", "setup b", "teardown a"]);
  });
})(window.AHTest, window.AH);
