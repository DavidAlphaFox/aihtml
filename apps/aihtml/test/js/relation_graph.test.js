/* relation_graph: the relation-graph behaviour (relation_graph.js) and
 * its nested chart on markup the server renders. SERVER holds a render
 * of aihtml_relation_graph:relation_graph/3 (circular, id "g1", a detail
 * card for "b"); regenerate it from Erlang if the markup changes. echarts
 * is loaded through AH.vendor as on a real page. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "graph": "<div class=\"ah-relation-graph\" role=\"group\" aria-roledescription=\"relation graph\" tabindex=\"0\" data-ah=\"relation-graph\" data-ah-value=\"\" data-layout=\"circular\" style=\"height:300px;\" id=\"g1\"><div class=\"ah-relation-graph__canvas ah-chart\" data-ah=\"chart\" aria-hidden=\"true\" style=\"height:100%;\"><script class=\"ah-chart-data\" type=\"application/json\">{\"series\":[{\"data\":[{\"id\":\"a\",\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"name\":\"Ann\",\"symbol\":\"circle\",\"symbolSize\":34},{\"id\":\"b\",\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"name\":\"Bob\",\"symbol\":\"circle\",\"symbolSize\":34},{\"id\":\"c\",\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"name\":\"Cy\",\"symbol\":\"circle\",\"symbolSize\":34}],\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"links\":[{\"value\":\"\",\"source\":\"a\",\"lineStyle\":{\"type\":\"solid\",\"width\":1.8,\"color\":\"--ah-color-border\",\"opacity\":0.9},\"target\":\"b\"},{\"value\":\"\",\"source\":\"b\",\"lineStyle\":{\"type\":\"solid\",\"width\":1.8,\"color\":\"--ah-color-border\",\"opacity\":0.9},\"target\":\"c\"}],\"type\":\"graph\",\"left\":40,\"right\":40,\"categories\":[],\"bottom\":40,\"top\":40,\"lineStyle\":{\"color\":\"--ah-color-border\",\"curveness\":0.08},\"emphasis\":{\"lineStyle\":{\"width\":3},\"focus\":\"adjacency\"},\"layout\":\"circular\",\"roam\":true,\"draggable\":true,\"edgeLabel\":{\"formatter\":\"{c}\",\"color\":\"--ah-color-text-secondary\",\"show\":false,\"fontSize\":11}}],\"color\":[\"--ah-color-primary\",\"--ah-color-success\",\"--ah-color-warning\",\"--ah-color-error\",\"--ah-color-info\",\"--ah-color-secondary\"],\"tooltip\":{\"trigger\":\"item\"},\"animationDuration\":400}</script></div><div class=\"ah-relation-graph__toolbar\" role=\"toolbar\"><button class=\"ah-relation-graph__tool\" type=\"button\" data-act=\"fit\" title=\"Fit view\" aria-label=\"Fit view\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M8 3H5a2 2 0 00-2 2v3M16 3h3a2 2 0 012 2v3M8 21H5a2 2 0 01-2-2v-3M16 21h3a2 2 0 002-2v-3\"/></svg></button><button class=\"ah-relation-graph__tool\" type=\"button\" data-act=\"refresh\" title=\"Refresh\" aria-label=\"Refresh\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M21 12a9 9 0 11-2.64-6.36M21 3v6h-6\"/></svg></button></div><div class=\"ah-relation-graph__detail\" data-visible=\"false\"><button class=\"ah-relation-graph__detail-close\" type=\"button\" aria-label=\"Close\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M18 6L6 18M6 6l12 12\"/></svg></button><div class=\"ah-relation-graph__detail-body\"><div class=\"ah-relation-graph__detail-item\" data-node=\"b\" hidden>Bob card</div></div></div><div class=\"ah-relation-graph__live\" aria-live=\"polite\" aria-atomic=\"true\"></div></div>"
  };

  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.firstChild;
  }

  // Resolves once the chart in `el` has drawn (ah:chart-ready).
  async function ready(el) {
    for (var i = 0; i < 100; i++) {
      if (AH.invoke(el, "instance")) { await sleep(50); return AH.invoke(el, "instance"); }
      await sleep(30);
    }
    throw new Error("chart never drew");
  }

  // A real pointer click at a pixel of the chart.
  function clickAt(el, x, y) {
    var target = el.querySelector("canvas").parentNode;
    var r = target.getBoundingClientRect();
    ["mousemove", "mousedown", "mouseup", "click"].forEach(function (type) {
      target.dispatchEvent(new MouseEvent(type, { bubbles: true, cancelable: true, view: window,
                                                  clientX: r.left + x, clientY: r.top + y }));
    });
  }

  T.test("relation-graph: click a node selects it, shows its card, fires ah:select once", async function (fx) {
    var el = mount(fx, "graph");
    var canvas = $(el).children(".ah-relation-graph__canvas")[0];
    var chart = await ready(canvas);
    var sel = [], clicks = [];
    $(el).on("ah:select", function (e, d) { sel.push(d.id); });
    $(el).on("ah:node-click", function (e, d) { clicks.push(d.id); });
    var layout = chart.getModel().getSeriesByIndex(0).getData().getItemLayout(1);
    var p = chart.convertToPixel({ seriesIndex: 0 }, layout);
    clickAt(canvas, p[0], p[1]);
    T.eq(clicks, ["b"]);
    T.eq(sel, ["b"]);
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq($(el).find(".ah-relation-graph__detail").attr("data-visible"), "true");
    T.eq($(el).find(".ah-relation-graph__detail-item[data-node=b]").attr("hidden"), undefined);
    T.eq($(el).find(".ah-relation-graph__live").text(), "Bob");
    clickAt(canvas, p[0], p[1]);
    T.eq(sel, ["b"], "same node again");
    $(el).find(".ah-relation-graph__detail-close").trigger("click");
    T.eq(sel, ["b", null]);
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq($(el).find(".ah-relation-graph__detail").attr("data-visible"), "false");
  });

  T.test("relation-graph: keyboard walks the nodes, Escape clears", async function (fx) {
    var el = mount(fx, "graph");
    await ready($(el).children(".ah-relation-graph__canvas")[0]);
    var sel = [];
    $(el).on("ah:select", function (e, d) { sel.push(d.id); });
    function key(k) { var e = $.Event("keydown", { key: k }); $(el).trigger(e); return e; }
    el.focus();
    T.ok(key("ArrowRight").isDefaultPrevented());
    T.eq(el.getAttribute("data-ah-value"), "a");
    key("ArrowRight");
    key("ArrowRight");
    key("ArrowRight");
    T.eq(el.getAttribute("data-ah-value"), "a", "wraps");
    key("ArrowLeft");
    T.eq(el.getAttribute("data-ah-value"), "c");
    key("Home");
    T.eq(el.getAttribute("data-ah-value"), "a");
    key("Escape");
    T.eq(sel, ["a", "b", "c", "a", "c", "a", null]);
    T.ok(!key("Escape").isDefaultPrevented(), "nothing to clear");
  });

  T.test("relation-graph: select / getSelected / fit / refresh", async function (fx) {
    var el = mount(fx, "graph");
    var canvas = $(el).children(".ah-relation-graph__canvas")[0];
    var chart = await ready(canvas);
    var sel = 0, refresh = 0;
    $(el).on("ah:select", function () { sel++; });
    $(el).on("ah:refresh", function () { refresh++; });
    AH.invoke(el, "select", "c");
    T.eq(AH.invoke(el, "getSelected"), "c");
    T.eq(sel, 0, "methods do not fire ah:select");
    T.eq($(el).find(".ah-relation-graph__detail").attr("data-visible"), "false", "no card for c");
    $(el).find("[data-act=refresh]").trigger("click");
    T.eq(refresh, 1);
    $(el).find("[data-act=fit]").trigger("click");
    T.eq(chart.getOption().series[0].data.length, 3);
    AH.invoke(el, "setOption", { series: [{ data: [{ id: "z", name: "Zed" }], links: [] }] });
    T.eq(chart.getOption().series[0].data.length, 1);
  });
  T.test("relation-graph: focus pans a node into the middle", async function (fx) {
    var el = mount(fx, "graph");
    var canvas = $(el).children(".ah-relation-graph__canvas")[0];
    var chart = await ready(canvas);
    function px(i) {
      var l = chart.getModel().getSeriesByIndex(0).getData().getItemLayout(i);
      return chart.convertToPixel({ seriesIndex: 0 }, l);
    }
    var before = px(2);
    AH.invoke(el, "focus", "c");
    await sleep(50);
    var after = px(2);
    T.ok(Math.abs(after[0] - chart.getWidth() / 2) < 2 && Math.abs(after[1] - chart.getHeight() / 2) < 2,
         "centred: " + before + " -> " + after);
  });
})(window.AHTest, window.jQuery, window.AH);
