/* treegrid: the treegrid behaviour on markup the server renders. SERVER
 * holds renders of aihtml_treegrid:treegrid/4 (ids "tg", multiple
 * selection with a load action, and "tgc", checkboxes); OPS the
 * operations treegrid_children/3 answers with for row "tg-1". Regenerate
 * them from Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "tg": "<div class=\"ah-tg\" id=\"tg\" role=\"treegrid\" aria-multiselectable=\"true\" data-ah=\"treegrid\" data-ah-value=\"\" data-selection=\"multiple\" data-sortable=\"true\" data-alt-rows=\"true\" data-load=\"g2gDdwFtdwRraWRzdAAAAAA.5z3LQpFX9OuAa5U_wOP7P8pw_dH5tNwEQyE1Mj06knI\"><div class=\"ah-tg-content\"><div class=\"ah-tg-header\"><table class=\"ah-tg-table\" role=\"presentation\"><colgroup><col style=\"width:200px;min-width:200px;\"><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-tg-header-row\" role=\"row\"><th class=\"ah-tg-th ah-tg-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-tg-th-content\"><span class=\"ah-tg-th-text\">Name</span><span class=\"ah-tg-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-tg-th ah-tg-th-sortable\" role=\"columnheader\" data-field=\"size\" data-type=\"number\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-tg-th-content\"><span class=\"ah-tg-th-text\">Size</span><span class=\"ah-tg-sort-icon\" aria-hidden=\"true\"></span></div></th></tr></thead></table></div><div class=\"ah-tg-body\"><table class=\"ah-tg-table\" role=\"presentation\"><colgroup><col style=\"width:200px;min-width:200px;\"><col></colgroup><tbody role=\"rowgroup\" id=\"tg-rows\"><tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tg-0\" role=\"row\" data-key=\"1\" data-parent=\"\" data-level=\"0\" data-i=\"0\" aria-level=\"1\" aria-expanded=\"false\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:0px;\"><span class=\"ah-tg-toggle ah-tg-toggle-closed\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>Docs</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"30\"><span>30</span></td></tr><tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-hover\" id=\"tg-0-0\" role=\"row\" data-key=\"2\" data-parent=\"1\" data-level=\"1\" data-i=\"0\" aria-level=\"2\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>b.txt</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"20\"><span>20</span></td></tr><tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tg-0-1\" role=\"row\" data-key=\"3\" data-parent=\"1\" data-level=\"1\" data-i=\"1\" aria-level=\"2\" aria-expanded=\"false\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\"><span class=\"ah-tg-toggle ah-tg-toggle-closed\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>a.txt</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"10\"><span>10</span></td></tr><tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-hover\" id=\"tg-0-1-0\" role=\"row\" data-key=\"4\" data-parent=\"3\" data-level=\"2\" data-i=\"0\" aria-level=\"3\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:48px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>x</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"5\"><span>5</span></td></tr><tr class=\"ah-tg-row ah-tg-row-alt ah-tg-row-hover\" id=\"tg-1\" role=\"row\" data-key=\"5\" data-parent=\"\" data-level=\"0\" data-i=\"1\" aria-level=\"1\" aria-expanded=\"false\" aria-selected=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-treegrid=\"tg\"><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:0px;\"><span class=\"ah-tg-toggle ah-tg-toggle-closed\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>Lazy</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"\"><span></span></td></tr><tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-hover\" id=\"tg-2\" role=\"row\" data-key=\"6\" data-parent=\"\" data-level=\"0\" data-i=\"2\" aria-level=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:0px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>z.md</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"1\"><span>1</span></td></tr><tr class=\"ah-tg-row-empty\" hidden><td class=\"ah-tg-cell-empty\" colspan=\"2\">No data to display</td></tr></tbody></table></div></div><input type=\"hidden\" name=\"sel\" value=\"\" data-ah-input></div>",
    "tgc": "<div class=\"ah-tg\" id=\"tgc\" role=\"treegrid\" aria-multiselectable=\"true\" data-ah=\"treegrid\" data-ah-value=\"\" data-selection=\"checkbox\" data-sortable=\"true\" data-alt-rows=\"true\"><div class=\"ah-tg-content\"><div class=\"ah-tg-header\"><table class=\"ah-tg-table\" role=\"presentation\"><colgroup><col style=\"width:40px;min-width:40px;\"><col style=\"width:200px;min-width:200px;\"><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-tg-header-row\" role=\"row\"><th class=\"ah-tg-th ah-tg-th-checkbox\" role=\"columnheader\"><input class=\"ah-tg-header-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select all rows\"></th><th class=\"ah-tg-th ah-tg-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-tg-th-content\"><span class=\"ah-tg-th-text\">Name</span><span class=\"ah-tg-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-tg-th ah-tg-th-sortable\" role=\"columnheader\" data-field=\"size\" data-type=\"number\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-tg-th-content\"><span class=\"ah-tg-th-text\">Size</span><span class=\"ah-tg-sort-icon\" aria-hidden=\"true\"></span></div></th></tr></thead></table></div><div class=\"ah-tg-body\"><table class=\"ah-tg-table\" role=\"presentation\"><colgroup><col style=\"width:40px;min-width:40px;\"><col style=\"width:200px;min-width:200px;\"><col></colgroup><tbody role=\"rowgroup\" id=\"tgc-rows\"><tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tgc-0\" role=\"row\" data-key=\"1\" data-parent=\"\" data-level=\"0\" data-i=\"0\" aria-level=\"1\" aria-expanded=\"true\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:0px;\"><span class=\"ah-tg-toggle ah-tg-toggle-open\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>Docs</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"30\"><span>30</span></td></tr><tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-alt ah-tg-row-hover\" id=\"tgc-0-0\" role=\"row\" data-key=\"2\" data-parent=\"1\" data-level=\"1\" data-i=\"0\" aria-level=\"2\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>b.txt</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"20\"><span>20</span></td></tr><tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tgc-0-1\" role=\"row\" data-key=\"3\" data-parent=\"1\" data-level=\"1\" data-i=\"1\" aria-level=\"2\" aria-expanded=\"true\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\"><span class=\"ah-tg-toggle ah-tg-toggle-open\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>a.txt</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"10\"><span>10</span></td></tr><tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-alt ah-tg-row-hover\" id=\"tgc-0-1-0\" role=\"row\" data-key=\"4\" data-parent=\"3\" data-level=\"2\" data-i=\"0\" aria-level=\"3\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:48px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>x</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"5\"><span>5</span></td></tr><tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tgc-1\" role=\"row\" data-key=\"5\" data-parent=\"\" data-level=\"0\" data-i=\"1\" aria-level=\"1\" aria-expanded=\"false\" aria-selected=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-treegrid=\"tgc\"><td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:0px;\"><span class=\"ah-tg-toggle ah-tg-toggle-closed\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>Lazy</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"\"><span></span></td></tr><tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-alt ah-tg-row-hover\" id=\"tgc-2\" role=\"row\" data-key=\"6\" data-parent=\"\" data-level=\"0\" data-i=\"2\" aria-level=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-tg-cell ah-tg-checkbox-cell\" role=\"gridcell\"><input class=\"ah-tg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:0px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>z.md</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"1\"><span>1</span></td></tr><tr class=\"ah-tg-row-empty\" hidden><td class=\"ah-tg-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div></div></div>"
  };

  var OPS = {
    "kids": [{"id":"tg-rows","op":"html","html":"<tr class=\"ah-tg-row ah-tg-row-leaf ah-tg-row-hover\" id=\"tg-1-0\" role=\"row\" data-key=\"50\" data-parent=\"5\" data-level=\"1\" data-i=\"0\" aria-level=\"2\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\"><span class=\"ah-tg-toggle ah-tg-toggle-leaf\" aria-hidden=\"true\"></span><span class=\"ah-tg-cell-text\"><span>c1</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"9\"><span>9</span></td></tr><tr class=\"ah-tg-row ah-tg-row-hover\" id=\"tg-1-1\" role=\"row\" data-key=\"51\" data-parent=\"5\" data-level=\"1\" data-i=\"1\" aria-level=\"2\" aria-expanded=\"false\" aria-selected=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-treegrid=\"tg\"><td class=\"ah-tg-cell ah-tg-tree-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><div class=\"ah-tg-tree-indent\" style=\"padding-left:24px;\"><span class=\"ah-tg-toggle ah-tg-toggle-closed\" aria-hidden=\"true\">▶</span><span class=\"ah-tg-cell-text\"><span>c2</span></span></div></td><td class=\"ah-tg-cell\" role=\"gridcell\" data-field=\"size\" style=\"text-align:left;\" data-value=\"2\"><span>2</span></td></tr>","swap":"append"},{"args":["tg-1"],"id":"tg","op":"call","method":"childrenLoaded"}]
  };

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.firstChild;
  }

  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  function key(el, k, extra) {
    var e = $.Event("keydown", $.extend({ key: k }, extra || {}));
    $(el).trigger(e);
    return e;
  }

  function rows(el, pre) { return $(el).find("tbody > tr." + pre + "-row").toArray(); }

  function keys(el, pre) { return rows(el, pre).map(function (r) { return r.getAttribute("data-key"); }); }

  function shown(el, pre) {
    return rows(el, pre).filter(function (r) { return !r.hidden; })
      .map(function (r) { return r.getAttribute("data-key"); });
  }

  function row(el, pre, k) { return $(el).find("tbody > tr." + pre + "-row[data-key='" + k + "']")[0]; }

  function focusedKey() { return document.activeElement && document.activeElement.getAttribute("data-key"); }

  // Answers the next actions with the given operations, as an AG-UI stream.
  function stubFetch(queue, calls) {
    var orig = window.fetch;
    window.fetch = function (url, opts) {
      calls.push(JSON.parse(opts.body));
      var ops = queue.shift() || [];
      var ev = function (o) { return "data: " + JSON.stringify(o) + "\n\n"; };
      var body = ev({ type: "RUN_STARTED" }) + ev({ type: "CUSTOM", name: "aihtml.ui", value: ops }) +
        ev({ type: "RUN_FINISHED" });
      return Promise.resolve(new Response(body, { status: 200, headers: { "Content-Type": "text/event-stream" } }));
    };
    return function () { window.fetch = orig; };
  }

  T.test("treegrid: expands, collapses and restripes", function (fx) {
    var el = mount(fx, "tg");
    var events = [];
    $(el).on("ah:expand ah:collapse", function (e, d) { events.push(e.type + ":" + d.key); });
    T.eq(shown(el, "ah-tg"), ["1", "5", "6"]);
    $(row(el, "ah-tg", "1")).find(".ah-tg-toggle").trigger("click");
    T.eq(shown(el, "ah-tg"), ["1", "2", "3", "5", "6"]);
    T.eq(row(el, "ah-tg", "1").getAttribute("aria-expanded"), "true");
    T.ok($(row(el, "ah-tg", "1")).find(".ah-tg-toggle").hasClass("ah-tg-toggle-open"));
    T.eq(el.getAttribute("data-key"), "1");
    T.eq(rows(el, "ah-tg").filter(function (r) { return !r.hidden; })
      .map(function (r) { return $(r).hasClass("ah-tg-row-alt") ? 1 : 0; }).join(""), "01010");
    T.eq(el.getAttribute("data-ah-value"), "", "the arrow does not select");
    $(row(el, "ah-tg", "3")).find(".ah-tg-toggle").trigger("click");
    T.eq(shown(el, "ah-tg"), ["1", "2", "3", "4", "5", "6"]);
    // collapsing the root hides the whole subtree, 3 stays open inside
    $(row(el, "ah-tg", "1")).find(".ah-tg-toggle").trigger("click");
    T.eq(shown(el, "ah-tg"), ["1", "5", "6"]);
    T.eq(row(el, "ah-tg", "3").getAttribute("aria-expanded"), "true");
    T.eq(events, ["ah:expand:1", "ah:expand:3", "ah:collapse:1"]);
  });

  T.test("treegrid: keyboard follows the treegrid pattern", function (fx) {
    var el = mount(fx, "tg");
    var first = row(el, "ah-tg", "1");
    T.eq(first.getAttribute("tabindex"), "0");
    first.focus();
    key(first, "ArrowRight");
    T.eq(first.getAttribute("aria-expanded"), "true", "right opens");
    key(first, "ArrowRight");
    T.eq(focusedKey(), "2", "right again moves to the first child");
    key(document.activeElement, "ArrowDown");
    T.eq(focusedKey(), "3");
    key(document.activeElement, "ArrowRight");
    key(document.activeElement, "ArrowRight");
    T.eq(focusedKey(), "4");
    key(document.activeElement, "ArrowLeft");
    T.eq(focusedKey(), "3", "left on a leaf moves to the parent");
    key(document.activeElement, "ArrowLeft");
    T.eq(row(el, "ah-tg", "3").getAttribute("aria-expanded"), "false", "left closes");
    key(document.activeElement, "End");
    T.eq(focusedKey(), "6");
    key(document.activeElement, "Home");
    T.eq(focusedKey(), "1");
    T.eq($(el).find("tbody > tr[tabindex='0']").length, 1);
    // collapsing the parent of the focused row brings the tab stop up
    key(document.activeElement, "ArrowDown");
    T.eq(focusedKey(), "2");
    AH.invoke(el, "collapse", "1");
    T.eq(row(el, "ah-tg", "1").getAttribute("tabindex"), "0");
    T.eq(focusedKey(), "1");
  });

  T.test("treegrid: multiple selection with Ctrl, Shift and Enter", function (fx) {
    var el = mount(fx, "tg");
    var changes = 0;
    $(el).on("change", function () { changes++; });
    AH.invoke(el, "expandAll");
    T.eq(shown(el, "ah-tg"), ["1", "2", "3", "4", "5", "6"], "lazy rows stay closed");
    $(row(el, "ah-tg", "2")).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "2");
    $(row(el, "ah-tg", "5")).trigger($.Event("click", { shiftKey: true }));
    T.eq(el.getAttribute("data-ah-value"), "2,3,4,5");
    $(row(el, "ah-tg", "3")).trigger($.Event("click", { ctrlKey: true }));
    $(row(el, "ah-tg", "4")).trigger($.Event("click", { ctrlKey: true }));
    T.eq(el.getAttribute("data-ah-value"), "2,5");
    T.eq($(el).children("input[name=sel]").val(), "2,5");
    T.eq(row(el, "ah-tg", "5").getAttribute("aria-selected"), "true");
    T.ok($(row(el, "ah-tg", "5")).hasClass("ah-tg-row-selected"));
    key(row(el, "ah-tg", "6"), "Enter");
    T.eq(el.getAttribute("data-ah-value"), "2,5,6");
    key(row(el, "ah-tg", "6"), " ");
    T.eq(el.getAttribute("data-ah-value"), "2,5");
    T.eq(changes, 6);
    AH.invoke(el, "setValue", ["1", "nope"]);
    T.eq(AH.invoke(el, "getValue"), "1");
    AH.invoke(el, "clearSelection");
    T.eq(AH.invoke(el, "getValue"), "");
    T.eq(changes, 6, "methods fire no change");
  });

  T.test("treegrid: sorts siblings and restores the order", function (fx) {
    var el = mount(fx, "tg");
    var sorts = [];
    $(el).on("ah:sort", function (e, d) { sorts.push(d.field + ":" + d.dir); });
    AH.invoke(el, "expandAll");
    AH.invoke(el, "expand", "3");
    var th = $(el).find("th[data-field=size]");
    th.trigger("click");
    T.eq(keys(el, "ah-tg"), ["6", "1", "3", "4", "2", "5"], "numbers first, children under parents");
    T.eq(th.attr("aria-sort"), "ascending");
    T.ok(th.hasClass("ah-tg-sort-asc"));
    T.eq(el.getAttribute("data-sort-field"), "size");
    th.trigger("click");
    T.eq(keys(el, "ah-tg"), ["5", "1", "2", "3", "4", "6"], "descending: the reverse, texts first");
    T.ok(th.hasClass("ah-tg-sort-desc"));
    th.trigger("click");
    T.eq(keys(el, "ah-tg"), ["1", "2", "3", "4", "5", "6"]);
    T.eq(th.attr("aria-sort"), undefined);
    T.eq(sorts, ["size:asc", "size:desc", "size:null"]);
    AH.invoke(el, "sort", "name", "asc");
    T.eq(keys(el, "ah-tg"), ["1", "3", "4", "2", "5", "6"]);
  });

  T.test("treegrid: lazy row loads its children from the server", async function (fx) {
    var el = mount(fx, "tg");
    var calls = [];
    var restore = stubFetch([OPS.kids], calls);
    try {
      var loads = [];
      $(el).on("ah:load", function (e, d) { loads.push(d.key); });
      $(el).on("ah:expand", function (e, d) { loads.push("open:" + d.key); });
      var lazy = row(el, "ah-tg", "5");
      $(lazy).find(".ah-tg-toggle").trigger("click");
      T.eq(loads, ["5"]);
      T.ok($(lazy).hasClass("ah-tg-row-loading"));
      T.eq(lazy.getAttribute("aria-busy"), "true");
      await wait(80);
      T.eq(calls.length, 1);
      T.eq(calls[0].event.type, "ah:load");
      T.eq(calls[0].event.id, "tg-1");
      T.eq([calls[0].event.data.key, calls[0].event.data.level, calls[0].event.data.treegrid],
           ["5", "0", "tg"]);
      T.eq(keys(el, "ah-tg"), ["1", "2", "3", "4", "5", "50", "51", "6"], "children moved under the row");
      T.eq(shown(el, "ah-tg"), ["1", "5", "50", "51", "6"]);
      T.eq(lazy.getAttribute("aria-expanded"), "true");
      T.ok(!lazy.hasAttribute("data-lazy") && !lazy.hasAttribute("aria-busy"));
      T.ok(!$(lazy).hasClass("ah-tg-row-loading"));
      T.eq(row(el, "ah-tg", "51").getAttribute("data-lazy"), "true");
      T.eq(loads, ["5", "open:5"]);
      // collapse and open again: no second request
      $(lazy).find(".ah-tg-toggle").trigger("click");
      $(lazy).find(".ah-tg-toggle").trigger("click");
      T.eq(calls.length, 1);
    } finally { restore(); }
  });

  T.test("treegrid: checkboxes select rows, the header all of them", function (fx) {
    var el = mount(fx, "tgc");
    var changes = 0;
    $(el).on("change", function () { changes++; });
    $(row(el, "ah-tg", "2")).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "", "a row click does not select in checkbox mode");
    $(row(el, "ah-tg", "2")).find(".ah-tg-row-checkbox").prop("checked", true).trigger("change");
    $(row(el, "ah-tg", "4")).find(".ah-tg-row-checkbox").prop("checked", true).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "2,4");
    T.ok($(el).find(".ah-tg-header-checkbox").prop("indeterminate"));
    $(el).find(".ah-tg-header-checkbox").prop("checked", true).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "1,2,3,4,5,6");
    T.eq($(el).find(".ah-tg-row-checkbox:checked").length, 6);
    $(el).find(".ah-tg-header-checkbox").prop("checked", false).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq(changes, 4);
  });
})(window.AHTest, window.jQuery, window.AH);
