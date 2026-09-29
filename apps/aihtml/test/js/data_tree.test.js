/* data_tree: tree, nav-tree and heatmap-calendar behaviours on markup the
 * server renders. SERVER holds renders of aihtml_data_tree:tree/4 (id "t",
 * animation none), nav_tree/4 (id "nt"), heatmap_calendar/3 (id "hm") and
 * the children set_children/3 renders for the lazy node "t-3"; regenerate
 * them from Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "tree": "<div class=\"ah-tree\" id=\"t\" role=\"tree\" data-ah=\"tree\" data-ah-value=\"\" data-toggle-mode=\"click\" data-animation=\"none\"><ul class=\"ah-tree-list\" role=\"presentation\"><li class=\"ah-tree-item\" id=\"t-0\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"0\" data-value=\"a\" aria-expanded=\"false\" tabindex=\"0\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Alpha</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-g\" style=\"display:none;\"><li class=\"ah-tree-item\" id=\"t-0-0\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"0-0\" data-value=\"a1\" aria-expanded=\"false\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">A one</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-0-0-g\" style=\"display:none;\"><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-0-0-0\" role=\"treeitem\" aria-level=\"3\" data-tree-id=\"0-0-0\" data-value=\"a1x\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">a1x</span></div></li></ul></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-0-1\" role=\"treeitem\" aria-level=\"2\" data-tree-id=\"0-1\" data-value=\"a2\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">A two</span></div></li></ul></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-1\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"1\" data-value=\"b\" tabindex=\"-1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Beta</span></div></li><li class=\"ah-tree-item ah-tree-item-leaf\" id=\"t-2\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"2\" data-value=\"Gone\" aria-disabled=\"true\" tabindex=\"-1\"><div class=\"ah-tree-row ah-tree-row-disabled\"><span class=\"ah-tree-toggle ah-tree-toggle-leaf\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Gone</span></div></li><li class=\"ah-tree-item\" id=\"t-3\" role=\"treeitem\" aria-level=\"1\" data-tree-id=\"3\" data-value=\"lz\" aria-expanded=\"false\" tabindex=\"-1\" data-lazy=\"true\" data-tree=\"t\" data-level=\"1\"><div class=\"ah-tree-row\"><span class=\"ah-tree-toggle\" aria-hidden=\"true\">â¶</span><span class=\"ah-tree-label\">Lazy</span></div><ul class=\"ah-tree-list\" role=\"group\" id=\"t-3-g\" style=\"display:none;\"></ul></li></ul><input type=\"hidden\" name=\"sel\" value=\"\" data-ah-input></div>",
    "nav": "<nav class=\"ah-nav-tree\" data-ah=\"nav-tree\" data-ah-value=\"home\" id=\"nt\"><div class=\"ah-nav-tree__group\"><div class=\"ah-nav-tree__group-label\">G</div><a class=\"ah-nav-tree__item ah-is-active\" href=\"#/home\" data-route=\"home\" aria-current=\"page\"><span class=\"ah-nav-tree__label\">Home</span></a><details class=\"ah-nav-tree__node\"><summary class=\"ah-nav-tree__item ah-nav-tree__item--parent\"><span class=\"ah-nav-tree__label\">U</span><span class=\"ah-nav-tree__caret\"><svg class=\"ah-nav-tree__caret-svg\" width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"m6 9 6 6 6-6\"/></svg></span></summary><div class=\"ah-nav-tree__children\"><div class=\"ah-nav-tree__children-inner\"><a class=\"ah-nav-tree__item\" href=\"#/u/p\" data-route=\"u/p\"><span class=\"ah-nav-tree__label\">P</span></a><a class=\"ah-nav-tree__item\" href=\"#/u/q\" data-route=\"u/q\"><span class=\"ah-nav-tree__label\">Q</span></a></div></div></details></div></nav>",
    "heat": "<div class=\"ah-heatmap-calendar\" data-ah=\"heatmap-calendar\" data-tip=\"{date}: {value}\" id=\"hm\"><div class=\"ah-heatmap-calendar__months\" aria-hidden=\"true\"><span class=\"ah-heatmap-calendar__month\" style=\"width:15px;\">Aug</span><span class=\"ah-heatmap-calendar__month\" style=\"width:75px;\">Sep</span></div><div class=\"ah-heatmap-calendar__body\"><div class=\"ah-heatmap-calendar__weekdays\" aria-hidden=\"true\"><span class=\"ah-heatmap-calendar__weekday\"></span><span class=\"ah-heatmap-calendar__weekday\">Mon</span><span class=\"ah-heatmap-calendar__weekday\"></span><span class=\"ah-heatmap-calendar__weekday\">Wed</span><span class=\"ah-heatmap-calendar__weekday\"></span><span class=\"ah-heatmap-calendar__weekday\">Fri</span><span class=\"ah-heatmap-calendar__weekday\"></span></div><div class=\"ah-heatmap-calendar__grid\"><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-23\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-24\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-25\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-26\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-27\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-28\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-29\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-30\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-08-31\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"2\" data-date=\"2026-09-01\" data-value=\"3\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-02\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-03\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-04\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-05\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-06\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-07\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-08\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-09\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-10\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-11\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-12\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-13\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-14\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-15\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-16\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-17\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-18\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-19\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-20\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-21\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-22\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-23\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-24\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-25\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-26\" data-value=\"0\"></div></div><div class=\"ah-heatmap-calendar__week\"><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-27\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-28\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-29\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-09-30\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-10-01\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-10-02\" data-value=\"0\"></div><div class=\"ah-heatmap-calendar__cell\" data-level=\"0\" data-date=\"2026-10-03\" data-value=\"0\"></div></div></div></div><div class=\"ah-heatmap-calendar__legend\" aria-hidden=\"true\"><span>Less</span><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"0\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"1\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"2\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"3\"></div><div class=\"ah-heatmap-calendar__legend-cell\" data-level=\"4\"></div><span>More</span></div><div class=\"ah-heatmap-calendar__tooltip\" data-visible=\"false\" role=\"tooltip\"></div></div>",
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

  T.test("nav-tree: following a link marks it and fires change", function (fx) {
    var el = mount(fx, "nav");
    var changes = 0;
    $(el).on("change", function () { changes++; });
    $(el).on("click", "a", function (e) { e.preventDefault(); });
    var q = $(el).find("a[data-route='u/q']")[0];
    $(q).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "u/q");
    T.ok($(q).hasClass("ah-is-active"));
    T.eq(q.getAttribute("aria-current"), "page");
    T.eq($(el).find(".ah-is-active").length, 1);
    T.eq($(el).find("details")[0].open, true);
    T.ok($(el).find("summary").hasClass("ah-is-open"));
    T.eq(changes, 1);
    $(q).trigger("click");
    T.eq(changes, 1);
    AH.invoke(el, "setValue", "home");
    T.eq($(el).find("details")[0].open, false);
    T.eq(AH.invoke(el, "getValue"), "home");
    T.eq(changes, 1);
  });

  T.test("heatmap-calendar: tooltip and select", function (fx) {
    var el = mount(fx, "heat");
    var cell = $(el).find("[data-date='2026-09-01']")[0];
    var tip = $(el).find(".ah-heatmap-calendar__tooltip")[0];
    $(cell).trigger("mouseenter");
    T.eq(tip.getAttribute("data-visible"), "true");
    T.eq(tip.textContent, "2026-09-01: 3");
    T.eq(tip.style.position, "fixed");
    $(cell).trigger("mouseleave");
    T.eq(tip.getAttribute("data-visible"), "false");
    var got = null;
    $(el).on("ah:select", function (e, d) { got = d; });
    $(cell).trigger("click");
    T.eq(got, { date: "2026-09-01", value: 3 });
    T.eq(el.getAttribute("data-ah-value"), "2026-09-01");
  });
})(window.AHTest, window.jQuery, window.AH);
