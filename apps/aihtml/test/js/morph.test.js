/* Morph swapping and focus preservation (core.js swap/morph). */
(function (T, $, AH) {
  "use strict";

  function html(fx, s) { fx.innerHTML = s; AH.mount(fx); }

  T.test("morph keeps the focused input, its caret and what was typed", function (fx) {
    html(fx, '<div id="box"><p class="msg">old</p><input id="q" value="server"></div>');
    var q = document.getElementById("q");
    q.focus();
    q.value = "typed by user";
    q.setSelectionRange(3, 5);
    AH.swap($("#box"), '<div id="box"><p class="msg new">new</p><input id="q" value="server2"></div>', "morph");
    T.ok(document.getElementById("q") === q, "same input node");
    T.eq(document.activeElement, q);
    T.eq(q.value, "typed by user");
    T.eq([q.selectionStart, q.selectionEnd], [3, 5]);
    T.eq($("#box .msg").text(), "new");
    T.eq($("#box .msg").attr("class"), "msg new");
  });

  T.test("morph updates an unfocused input's value from the server", function (fx) {
    html(fx, '<div id="box"><input id="a" value="1"><input id="b" type="checkbox"></div>');
    AH.swap($("#box"), '<div id="box"><input id="a" value="2"><input id="b" type="checkbox" checked></div>', "morph");
    T.eq(document.getElementById("a").value, "2");
    T.eq(document.getElementById("b").checked, true);
  });

  T.test("morph matches by id, so reordered nodes keep their identity", function (fx) {
    html(fx, '<ul id="l"><li id="x">x</li><li id="y">y</li><li id="z">z</li></ul>');
    var y = document.getElementById("y");
    AH.swap($("#l"), '<ul id="l"><li id="y">Y</li><li id="z">z</li><li id="w">w</li></ul>', "morph");
    T.ok(document.getElementById("y") === y, "y kept");
    T.eq($("#l li").map(function () { return this.id + ":" + this.textContent; }).get(),
         ["y:Y", "z:z", "w:w"]);
  });

  T.test("morph_inner patches the children and drops removed ones", function (fx) {
    html(fx, '<div id="box"><span>a</span><b>b</b><i>c</i></div>');
    var span = fx.querySelector("span");
    AH.swap($("#box"), "<span>A</span><em>e</em>", "morph_inner");
    T.ok(fx.querySelector("span") === span, "span kept");
    T.eq(document.getElementById("box").innerHTML, "<span>A</span><em>e</em>");
  });

  T.test("morph re-initialises a changed component and mounts added ones", function (fx) {
    var inits = [], destroys = [];
    AH.define("t-morph", {
      init: function (el) { inits.push(el.getAttribute("data-v")); },
      destroy: function (el) { destroys.push(el.getAttribute("data-v")); }
    });
    html(fx, '<div id="box"><div data-ah="t-morph" id="c1" data-v="1"></div></div>');
    T.eq(inits, ["1"]);
    var c1 = document.getElementById("c1");
    AH.swap($("#box"), '<div id="box"><div data-ah="t-morph" id="c1" data-v="2"></div>' +
                       '<div data-ah="t-morph" id="c2" data-v="new"></div></div>', "morph");
    T.ok(document.getElementById("c1") === c1, "component node kept");
    T.eq(destroys, ["2"]);          // destroyed after its attributes were synced
    T.eq(inits, ["1", "2", "new"]);
    T.ok(c1.hasAttribute("data-ah-mounted"), "still mounted");
  });

  T.test("an unchanged component is left alone", function (fx) {
    var inits = 0;
    AH.define("t-still", { init: function () { inits++; } });
    html(fx, '<div id="box"><div data-ah="t-still" id="s"></div><p>1</p></div>');
    AH.swap($("#box"), '<div id="box"><div data-ah="t-still" id="s"></div><p>2</p></div>', "morph");
    T.eq(inits, 1);
  });

  T.test("a plain inner swap puts the focus back by id", function (fx) {
    html(fx, '<div id="box"><input id="f" value="hello"></div>');
    var f = document.getElementById("f");
    f.focus();
    f.setSelectionRange(1, 4);
    AH.swap($("#box"), '<input id="f" value="hello">', "inner");
    var g = document.getElementById("f");
    T.ok(g !== f, "node was replaced");
    T.eq(document.activeElement, g);
    T.eq([g.selectionStart, g.selectionEnd], [1, 4]);
  });

  T.test("morph (outer) needs a single root", function (fx) {
    html(fx, '<div id="box"></div>');
    var threw = false;
    try { AH.swap($("#box"), "<p>a</p><p>b</p>", "morph"); } catch (e) { threw = true; }
    T.ok(threw);
  });

  T.test("server ops can morph", function (fx) {
    html(fx, '<div id="box"><input id="k" value="v"></div>');
    var k = document.getElementById("k");
    k.focus();
    AH.apply([{ op: "html", id: "box", swap: "morph", html: '<div id="box" class="done"><input id="k" value="v"></div>' }]);
    T.ok(document.getElementById("k") === k);
    T.eq(document.activeElement, k);
    T.eq(document.getElementById("box").className, "done");
  });
})(window.AHTest, window.jQuery, window.AH);
