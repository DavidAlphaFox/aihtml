/* tree: the tree behaviour on markup the server renders. SERVER holds
 * renders of aihtml_tree:tree/4 (id "t", animation none) and the children
 * set_children/3 renders for the lazy node "t-3"; regenerate them from
 * Erlang if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "tree": "<div class=\"ah-tree\" id=\"t\" role=\"tree\" data-ah=\"tree\" data-ah-value=\"\" data-toggle-mode=\"click\" data-animation=\"none\"><ul class=\"ah-tree-list\" role=\"presentation\"><li class=\"ah-tree-item\" id=\"t-0\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"0\" data-value=\"a\" aria-expanded=\"false\" tabindex=\"0\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Alpha</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-g\" style=\"display:none;\"><li class=\"ah-tree-item\" id=\"t-0-0\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"0-0\" data-value=\"a1\" aria-expanded=\"false\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">A one</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-0-g\" style=\"display:none;\"><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-0-0-0\" role=\"treeitem\" aria-level=\"3\" data-tree-id=\"0-0-0\" data-value=\"a1x\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">a1x</span></div></li></ul></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-0-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"0-1\" data-value=\"a2\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">A two</span></div></li></ul></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-1\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"1\" data-value=\"b\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Beta</span></div></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-2\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"2\" data-value=\"Gone\" aria-disabled=\"true\" tabindex=\"-1\"><div class=\"ah-tree-row ah-tree-row-disabled\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Gone</span></div></li><li class=\"ah-tree-item\" id=\"t-3\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"3\" data-value=\"lz\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-tree=\"t\" data-level=\"1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Lazy</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-3-g\" style=\"display:none;\"></ul></li></ul><input type=\"hidden\" name=\"sel\" value=\"\" data-ah-input></div>",
    "lazykids": "<li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-3-0\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"3-0\" data-value=\"x1\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">x1</span></div></li><li class=\"ah-tree-item\" id=\"t-3-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"3-1\" data-value=\"x2\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-tree=\"t\" data-level=\"2\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">x2</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-3-1-g\" style=\"display:none;\"></ul></li>"
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.firstChild;
  }

  function li(id) { return document.getElementById(id); }

  function key(el, k) { return T.key(el, k); }
  function row(id) { return li(id).querySelector(":scope > .ah-tree-row"); }
  function display(id) { return getComputedStyle(document.getElementById(id)).display; }
  function on(el, type, fn) { el.addEventListener(type, fn); }

  function focused() { return document.activeElement && document.activeElement.id; }

  function tabStops(el) {
    return Array.prototype.map.call(el.querySelectorAll("li[tabindex='0']"), function (n) { return n.id; });
  }

  T.test("tree: click selects, toggles and fires change once per new value", async function (fx) {
    var el = await mount(fx, "tree");
    var changes = 0, expands = [];
    on(el, "change", function () { changes++; });
    on(el, "ah:expand", function (e) { expands.push(e.detail.value); });
    T.eq(tabStops(el), ["t-0"]);
    row("t-0").click();
    T.eq(li("t-0").getAttribute("aria-expanded"), "true");
    T.eq(display("t-0-g"), "block");
    T.eq(expands, ["a"]);
    T.eq(el.getAttribute("data-ah-value"), "a");
    T.eq(el.querySelector(":scope > input[name=sel]").value, "a");
    T.eq(li("t-0").getAttribute("aria-selected"), "true");
    T.ok(row("t-0").classList.contains("ah-tree-row-selected"));
    T.eq(changes, 1);
    row("t-0-1").click();
    T.eq(el.getAttribute("data-ah-value"), "a2");
    T.eq(li("t-0").hasAttribute("aria-selected"), false);
    T.eq(tabStops(el), ["t-0-1"]);
    T.eq(changes, 2);
    row("t-0-1").click();
    T.eq(changes, 2, "same value again");
    // disabled rows do nothing
    row("t-2").click();
    T.eq(el.getAttribute("data-ah-value"), "a2");
  });

  T.test("tree: keyboard moves the roving tab stop", async function (fx) {
    var el = await mount(fx, "tree");
    li("t-0").focus();
    key(li("t-0"), "ArrowRight");
    T.eq(li("t-0").getAttribute("aria-expanded"), "true", "right opens");
    key(li("t-0"), "ArrowRight");
    T.eq(focused(), "t-0-0", "right again enters");
    key(document.activeElement, "ArrowDown");
    T.eq(focused(), "t-0-1", "a1 is closed: down skips its children");
    key(document.activeElement, "ArrowDown");
    T.eq(focused(), "t-1");
    key(document.activeElement, "End");
    T.eq(focused(), "t-3");
    key(document.activeElement, "Home");
    T.eq(focused(), "t-0");
    key(document.activeElement, "ArrowDown");
    key(document.activeElement, "ArrowLeft");
    T.eq(focused(), "t-0", "left goes to the parent");
    key(document.activeElement, "ArrowLeft");
    T.eq(li("t-0").getAttribute("aria-expanded"), "false", "left closes");
    key(document.activeElement, "b");
    T.eq(focused(), "t-1", "type-ahead");
    var changed = 0;
    on(el, "change", function () { changed++; });
    key(document.activeElement, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq(changed, 1);
    T.eq(tabStops(el), ["t-1"]);
    key(document.activeElement, "*");
    T.eq(li("t-0").getAttribute("aria-expanded"), "true", "* opens siblings");
  });

  T.test("tree: methods", async function (fx) {
    var el = await mount(fx, "tree");
    var changed = 0;
    on(el, "change", function () { changed++; });
    AH.invoke(el, "setValue", "a1x");
    T.eq(li("t-0").getAttribute("aria-expanded"), "true");
    T.eq(li("t-0-0").getAttribute("aria-expanded"), "true");
    T.eq(li("t-0-0-0").getAttribute("aria-selected"), "true");
    T.eq(AH.invoke(el, "getValue"), "a1x");
    T.eq(changed, 0);
    AH.invoke(el, "collapseAll");
    T.eq(el.querySelectorAll("li[aria-expanded=true]").length, 0);
    T.eq(tabStops(el), ["t-0"], "the collapsed node takes the tab stop");
    AH.invoke(el, "expandAll");
    T.eq(el.querySelectorAll("li[aria-expanded=true]").length, 2, "lazy node stays closed");
    AH.invoke(el, "collapse", "a");
    T.eq(li("t-0").getAttribute("aria-expanded"), "false");
    AH.invoke(el, "setValue", "nope");
    T.eq(AH.invoke(el, "getValue"), "");
    T.eq(el.querySelectorAll("[aria-selected]").length, 0);
  });

  T.test("tree: a lazy node loads, then expands", async function (fx) {
    var el = await mount(fx, "tree");
    var loads = [];
    on(el, "ah:load", function (e) { loads.push(e.detail.value); });
    row("t-3").click();
    T.eq(loads, ["lz"]);
    T.ok(li("t-3").classList.contains("ah-tree-item-loading"));
    T.eq(li("t-3").getAttribute("aria-expanded"), "false", "not open before the answer");
    row("t-3").click();
    T.eq(loads, ["lz"], "one request at a time");
    // what set_children sends: the children morphed in, then childrenLoaded
    document.getElementById("t-3-g").innerHTML = SERVER.lazykids;
    AH.invoke(el, "childrenLoaded", "t-3");
    T.eq(li("t-3").getAttribute("aria-expanded"), "true");
    T.eq(li("t-3").classList.contains("ah-tree-item-loading"), false);
    T.eq(li("t-3").hasAttribute("data-lazy"), false);
    T.eq(display("t-3-g"), "block");
    T.eq(li("t-3-1").getAttribute("data-lazy"), "true", "nested lazy node");
    // collapse and reopen without loading again
    row("t-3").click();
    row("t-3").click();
    T.eq(loads, ["lz"]);
    // an empty answer makes a leaf
    row("t-3-1").click();
    AH.invoke(el, "childrenLoaded", "t-3-1");
    T.eq(li("t-3-1").hasAttribute("aria-expanded"), false);
    T.ok(li("t-3-1").classList.contains("ah-tree-item-leaf"));
  });

  T.test("tree: slide animation fires ah:expand when done", async function (fx) {
    fx.innerHTML = SERVER.tree.replace('data-animation="none"', 'data-animation="slide"');
    await T.ready(fx);
    var el = fx.firstChild;
    var done = new Promise(function (res) { on(el, "ah:expand", function () { res(); }); });
    AH.invoke(el, "expand", "a");
    await done;
    T.eq(display("t-0-g"), "block");
  });

  T.test("tree: a slide still running jumps to its end; collapse hides", async function (fx) {
    fx.innerHTML = SERVER.tree.replace('data-animation="none"', 'data-animation="slide"');
    await T.ready(fx);
    var el = fx.firstChild, seen = [];
    on(el, "ah:expand", function (e) { seen.push("+" + e.detail.value); });
    on(el, "ah:collapse", function (e) { seen.push("-" + e.detail.value); });
    AH.invoke(el, "expand", "a");
    AH.invoke(el, "collapse", "a");
    T.eq(seen, ["+a"], "the running slide finished first");
    await new Promise(function (res) { setTimeout(res, 320); });
    T.eq(seen, ["+a", "-a"]);
    T.eq(display("t-0-g"), "none");
  });

  T.test("tree: the lazy load reaches the ah:load binding; re-insertion works", async function (fx) {
    var el = await mount(fx, "tree");
    var got = null;
    document.addEventListener("ah:load", function h(e) {
      got = e.target.id;
      document.removeEventListener("ah:load", h);
    });
    el.setAttribute("data-load", "x.y");
    row("t-3").click();
    T.eq(got, "t-3");
    T.eq(li("t-3").getAttribute("data-ah-on"), "ah:load:x.y");
    el.remove();
    await new Promise(function (res) { setTimeout(res, 0); });
    el = await mount(fx, "tree");
    row("t-1").click();
    T.eq(AH.invoke(el, "getValue"), "b");
  });
})(window.AHTest, window.AH);
