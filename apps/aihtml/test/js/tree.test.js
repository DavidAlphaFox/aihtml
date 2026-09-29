/* tree: the tree behaviour on markup the server renders. SERVER holds
 * renders of aihtml_tree:tree/4 (id "t", animation none) and the children
 * set_children/3 renders for the lazy node "t-3"; regenerate them from
 * Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "tree": "<div class=\"ah-tree\" id=\"t\" role=\"tree\" data-ah=\"tree\" data-ah-value=\"\" data-toggle-mode=\"click\" data-animation=\"none\"><ul class=\"ah-tree-list\" role=\"presentation\"><li class=\"ah-tree-item\" id=\"t-0\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"0\" data-value=\"a\" aria-expanded=\"false\" tabindex=\"0\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Alpha</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-g\" style=\"display:none;\"><li class=\"ah-tree-item\" id=\"t-0-0\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"0-0\" data-value=\"a1\" aria-expanded=\"false\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">A one</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-0-g\" style=\"display:none;\"><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-0-0-0\" role=\"treeitem\" aria-level=\"3\" data-tree-id=\"0-0-0\" data-value=\"a1x\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">a1x</span></div></li></ul></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-0-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"0-1\" data-value=\"a2\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">A two</span></div></li></ul></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-1\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"1\" data-value=\"b\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Beta</span></div></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-2\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"2\" data-value=\"Gone\" aria-disabled=\"true\" tabindex=\"-1\"><div class=\"ah-tree-row ah-tree-row-disabled\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Gone</span></div></li><li class=\"ah-tree-item\" id=\"t-3\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"3\" data-value=\"lz\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-tree=\"t\" data-level=\"1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Lazy</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-3-g\" style=\"display:none;\"></ul></li></ul><input type=\"hidden\" name=\"sel\" value=\"\" data-ah-input></div>",
    "lazykids": "<li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-3-0\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"3-0\" data-value=\"x1\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">x1</span></div></li><li class=\"ah-tree-item\" id=\"t-3-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"3-1\" data-value=\"x2\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-tree=\"t\" data-level=\"2\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">x2</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-3-1-g\" style=\"display:none;\"></ul></li>"
  };

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.firstChild;
  }

  function li(id) { return document.getElementById(id); }

  function key(el, k) {
    var e = $.Event("keydown", { key: k });
    $(el).trigger(e);
    return e;
  }

  function focused() { return document.activeElement && document.activeElement.id; }

  function tabStops(el) { return $(el).find("li[tabindex='0']").map(function () { return this.id; }).get(); }

  T.test("tree: click selects, toggles and fires change once per new value", function (fx) {
    var el = mount(fx, "tree");
    var changes = 0, expands = [];
    $(el).on("change", function () { changes++; });
    $(el).on("ah:expand", function (e, d) { expands.push(d.value); });
    T.eq(tabStops(el), ["t-0"]);
    $(li("t-0")).children(".ah-tree-row").trigger("click");
    T.eq(li("t-0").getAttribute("aria-expanded"), "true");
    T.eq($("#t-0-g").css("display"), "block");
    T.eq(expands, ["a"]);
    T.eq(el.getAttribute("data-ah-value"), "a");
    T.eq($(el).children("input[name=sel]").val(), "a");
    T.eq(li("t-0").getAttribute("aria-selected"), "true");
    T.ok($(li("t-0")).children(".ah-tree-row").hasClass("ah-tree-row-selected"));
    T.eq(changes, 1);
    $(li("t-0-1")).children(".ah-tree-row").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "a2");
    T.eq(li("t-0").hasAttribute("aria-selected"), false);
    T.eq(tabStops(el), ["t-0-1"]);
    T.eq(changes, 2);
    $(li("t-0-1")).children(".ah-tree-row").trigger("click");
    T.eq(changes, 2, "same value again");
    // disabled rows do nothing
    $(li("t-2")).children(".ah-tree-row").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "a2");
  });

  T.test("tree: keyboard moves the roving tab stop", function (fx) {
    var el = mount(fx, "tree");
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
    $(el).on("change", function () { changed++; });
    key(document.activeElement, "Enter");
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq(changed, 1);
    T.eq(tabStops(el), ["t-1"]);
    key(document.activeElement, "*");
    T.eq(li("t-0").getAttribute("aria-expanded"), "true", "* opens siblings");
  });

  T.test("tree: methods", function (fx) {
    var el = mount(fx, "tree");
    var changed = 0;
    $(el).on("change", function () { changed++; });
    AH.invoke(el, "setValue", "a1x");
    T.eq(li("t-0").getAttribute("aria-expanded"), "true");
    T.eq(li("t-0-0").getAttribute("aria-expanded"), "true");
    T.eq(li("t-0-0-0").getAttribute("aria-selected"), "true");
    T.eq(AH.invoke(el, "getValue"), "a1x");
    T.eq(changed, 0);
    AH.invoke(el, "collapseAll");
    T.eq($(el).find("li[aria-expanded=true]").length, 0);
    T.eq(tabStops(el), ["t-0"], "the collapsed node takes the tab stop");
    AH.invoke(el, "expandAll");
    T.eq($(el).find("li[aria-expanded=true]").length, 2, "lazy node stays closed");
    AH.invoke(el, "collapse", "a");
    T.eq(li("t-0").getAttribute("aria-expanded"), "false");
    AH.invoke(el, "setValue", "nope");
    T.eq(AH.invoke(el, "getValue"), "");
    T.eq($(el).find("[aria-selected]").length, 0);
  });

  T.test("tree: a lazy node loads, then expands", function (fx) {
    var el = mount(fx, "tree");
    var loads = [];
    $(el).on("ah:load", function (e, d) { loads.push(d.value); });
    $(li("t-3")).children(".ah-tree-row").trigger("click");
    T.eq(loads, ["lz"]);
    T.ok($(li("t-3")).hasClass("ah-tree-item-loading"));
    T.eq(li("t-3").getAttribute("aria-expanded"), "false", "not open before the answer");
    $(li("t-3")).children(".ah-tree-row").trigger("click");
    T.eq(loads, ["lz"], "one request at a time");
    // what set_children sends: the children morphed in, then childrenLoaded
    $("#t-3-g").html(SERVER.lazykids);
    AH.invoke(el, "childrenLoaded", "t-3");
    T.eq(li("t-3").getAttribute("aria-expanded"), "true");
    T.eq($(li("t-3")).hasClass("ah-tree-item-loading"), false);
    T.eq(li("t-3").hasAttribute("data-lazy"), false);
    T.eq($("#t-3-g").css("display"), "block");
    T.eq(li("t-3-1").getAttribute("data-lazy"), "true", "nested lazy node");
    // collapse and reopen without loading again
    $(li("t-3")).children(".ah-tree-row").trigger("click");
    $(li("t-3")).children(".ah-tree-row").trigger("click");
    T.eq(loads, ["lz"]);
    // an empty answer makes a leaf
    $(li("t-3-1")).children(".ah-tree-row").trigger("click");
    AH.invoke(el, "childrenLoaded", "t-3-1");
    T.eq(li("t-3-1").hasAttribute("aria-expanded"), false);
    T.ok($(li("t-3-1")).hasClass("ah-tree-item-leaf"));
  });

  T.test("tree: slide animation fires ah:expand when done", async function (fx) {
    fx.innerHTML = SERVER.tree.replace('data-animation="none"', 'data-animation="slide"');
    AH.mount(fx);
    var el = fx.firstChild;
    var done = new Promise(function (res) { $(el).on("ah:expand", function () { res(); }); });
    AH.invoke(el, "expand", "a");
    await done;
    T.eq($("#t-0-g").css("display"), "block");
  });
})(window.AHTest, window.jQuery, window.AH);
