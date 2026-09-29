/* data_charts: chart and relation-graph behaviours on markup the server
 * renders. SERVER holds renders of aihtml_data_charts:bar_chart/3 (id
 * "c1"), chart/3 with a "var(--ah-color-primary)" colour (id "c2") and
 * relation_graph/3 (circular, id "g1", a detail card for "b"); regenerate
 * them from Erlang if the markup changes. echarts is loaded through
 * AH.vendor as on a real page. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "bar": "<div class=\"ah-chart\" role=\"img\" data-ah=\"chart\" style=\"height:240px;\" id=\"c1\" aria-label=\"Bars\"><script class=\"ah-chart-data\" type=\"application/json\">{\"xAxis\":{\"data\":[\"a\",\"b\",\"c\"],\"type\":\"category\",\"axisLine\":{\"show\":true},\"axisTick\":{\"show\":false}},\"yAxis\":{\"type\":\"value\",\"axisLine\":{\"show\":false},\"axisTick\":{\"show\":false},\"splitLine\":{\"lineStyle\":{\"type\":\"dashed\"},\"show\":true}},\"series\":[{\"data\":[5,9,4],\"name\":\"Q1\",\"type\":\"bar\",\"itemStyle\":{\"borderRadius\":[4,4,0,0]},\"barMaxWidth\":40}],\"tooltip\":{\"axisPointer\":{\"type\":\"shadow\"},\"trigger\":\"axis\"},\"grid\":{\"left\":12,\"right\":20,\"bottom\":12,\"top\":16,\"containLabel\":true}}</script></div>",
    "graph": "<div class=\"ah-relation-graph\" role=\"group\" aria-roledescription=\"relation graph\" tabindex=\"0\" data-ah=\"relation-graph\" data-ah-value=\"\" data-layout=\"circular\" style=\"height:300px;\" id=\"g1\"><div class=\"ah-relation-graph__canvas ah-chart\" data-ah=\"chart\" aria-hidden=\"true\" style=\"height:100%;\"><script class=\"ah-chart-data\" type=\"application/json\">{\"series\":[{\"data\":[{\"id\":\"a\",\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"name\":\"Ann\",\"symbol\":\"circle\",\"symbolSize\":34},{\"id\":\"b\",\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"name\":\"Bob\",\"symbol\":\"circle\",\"symbolSize\":34},{\"id\":\"c\",\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"name\":\"Cy\",\"symbol\":\"circle\",\"symbolSize\":34}],\"label\":{\"position\":\"inside\",\"color\":\"--ah-color-primary-contrast\",\"show\":true,\"fontSize\":12,\"overflow\":\"truncate\"},\"links\":[{\"value\":\"\",\"source\":\"a\",\"lineStyle\":{\"type\":\"solid\",\"width\":1.8,\"color\":\"--ah-color-border\",\"opacity\":0.9},\"target\":\"b\"},{\"value\":\"\",\"source\":\"b\",\"lineStyle\":{\"type\":\"solid\",\"width\":1.8,\"color\":\"--ah-color-border\",\"opacity\":0.9},\"target\":\"c\"}],\"type\":\"graph\",\"left\":40,\"right\":40,\"categories\":[],\"bottom\":40,\"top\":40,\"lineStyle\":{\"color\":\"--ah-color-border\",\"curveness\":0.08},\"emphasis\":{\"lineStyle\":{\"width\":3},\"focus\":\"adjacency\"},\"layout\":\"circular\",\"roam\":true,\"draggable\":true,\"edgeLabel\":{\"formatter\":\"{c}\",\"color\":\"--ah-color-text-secondary\",\"show\":false,\"fontSize\":11}}],\"color\":[\"--ah-color-primary\",\"--ah-color-success\",\"--ah-color-warning\",\"--ah-color-error\",\"--ah-color-info\",\"--ah-color-secondary\"],\"tooltip\":{\"trigger\":\"item\"},\"animationDuration\":400}</script></div><div class=\"ah-relation-graph__toolbar\" role=\"toolbar\"><button class=\"ah-relation-graph__tool\" type=\"button\" data-act=\"fit\" title=\"Fit view\" aria-label=\"Fit view\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M8 3H5a2 2 0 00-2 2v3M16 3h3a2 2 0 012 2v3M8 21H5a2 2 0 01-2-2v-3M16 21h3a2 2 0 002-2v-3\"/></svg></button><button class=\"ah-relation-graph__tool\" type=\"button\" data-act=\"refresh\" title=\"Refresh\" aria-label=\"Refresh\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M21 12a9 9 0 11-2.64-6.36M21 3v6h-6\"/></svg></button></div><div class=\"ah-relation-graph__detail\" data-visible=\"false\"><button class=\"ah-relation-graph__detail-close\" type=\"button\" aria-label=\"Close\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M18 6L6 18M6 6l12 12\"/></svg></button><div class=\"ah-relation-graph__detail-body\"><div class=\"ah-relation-graph__detail-item\" data-node=\"b\" hidden>Bob card</div></div></div><div class=\"ah-relation-graph__live\" aria-live=\"polite\" aria-atomic=\"true\"></div></div>",
    "tokens": "<div class=\"ah-chart\" role=\"img\" data-ah=\"chart\" style=\"height:200px;\" id=\"c2\"><script class=\"ah-chart-data\" type=\"application/json\">{\"xAxis\":{\"data\":[\"a\"],\"type\":\"category\"},\"yAxis\":{\"type\":\"value\"},\"series\":[{\"data\":[{\"value\":3,\"itemStyle\":{\"color\":\"var(--ah-color-primary)\"}}],\"type\":\"bar\"}]}</script></div>"
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

  function painted(el) {
    var cv = el.querySelector("canvas");
    var d = cv.getContext("2d").getImageData(0, 0, cv.width, cv.height).data, n = 0;
    for (var i = 3; i < d.length; i += 4) { if (d[i] > 0) { n++; } }
    return n;
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

  T.test("chart: draws the server's option, keeps the data island, aria", async function (fx) {
    var el = mount(fx, "bar");
    var chart = await ready(el);
    T.ok(painted(el) > 1000, "canvas has pixels");
    var o = chart.getOption();
    T.eq(o.series[0].data, [5, 9, 4]);
    T.eq(o.xAxis[0].data, ["a", "b", "c"]);
    T.eq($(el).children("script.ah-chart-data").length, 1, "island kept across init");
    T.eq([].concat(o.aria)[0].enabled, true);
    T.eq(el.getAttribute("aria-label"), "Bars");
  });

  T.test("chart: a click fires ah:chart-click with the item in data-* and data-ah-value", async function (fx) {
    var el = mount(fx, "bar");
    var chart = await ready(el);
    var got = [];
    $(el).on("ah:chart-click", function (e, info) { got.push(info); });
    var p = chart.convertToPixel({ seriesIndex: 0 }, ["b", 5]);
    clickAt(el, p[0], p[1]);
    T.eq(got.length, 1);
    T.eq(got[0].name, "b");
    T.eq(got[0].value, 9);
    T.eq(got[0].seriesName, "Q1");
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq(el.getAttribute("data-series"), "Q1");
    T.eq(el.getAttribute("data-value"), "9");
    T.eq(el.getAttribute("data-index"), "1");
    T.eq(el.getAttribute("data-kind"), "series");
  });

  T.test("chart: setOption merges, setData replaces series data, methods queue before load", async function (fx) {
    var el = mount(fx, "bar");
    // called before echarts has loaded: queued
    AH.invoke(el, "setData", [[1, 2, 3]]);
    var chart = await ready(el);
    T.eq(chart.getOption().series[0].data, [1, 2, 3]);
    AH.invoke(el, "setOption", { series: [{ data: [7, 7, 7] }] });
    T.eq(chart.getOption().series[0].data, [7, 7, 7]);
    T.eq(chart.getOption().xAxis[0].data, ["a", "b", "c"], "merged");
    AH.invoke(el, "setOption", { xAxis: { type: "category", data: ["x"] }, yAxis: {},
                                 series: [{ type: "line", data: [1] }] }, true);
    T.eq(chart.getOption().series[0].type, "line", "replaced");
    AH.invoke(el, "showLoading");
    AH.invoke(el, "hideLoading");
    T.ok(/^data:image\/png/.test(AH.invoke(el, "getDataURL")));
  });

  T.test("chart: theme tokens resolve and follow a theme change", async function (fx) {
    fx.style.setProperty("--ah-color-primary", "#ff0000");
    fx.style.setProperty("--ah-color-text-secondary", "#00ff00");
    var el = mount(fx, "tokens");
    var chart = await ready(el);
    var o = chart.getOption();
    T.eq(o.series[0].data[0].itemStyle.color, "rgb(255,0,0)");
    T.eq(o.color[0], "rgb(255,0,0)", "palette from the theme");
    T.eq(o.xAxis[0].axisLabel.color, "rgb(0,255,0)", "axis text from the theme");
    fx.style.setProperty("--ah-color-primary", "#0000ff");
    $(document).trigger("ah:theme", [{ axis: "palette", value: "x" }]);
    await sleep(50);
    o = chart.getOption();
    T.eq(o.series[0].data[0].itemStyle.color, "rgb(0,0,255)");
    T.eq(o.color[0], "rgb(0,0,255)");
    T.eq(o.xAxis[0].axisLabel.color, "rgb(0,255,0)", "unchanged token stays");
    fx.style.removeProperty("--ah-color-primary");
    fx.style.removeProperty("--ah-color-text-secondary");
  });

  T.test("chart: destroy disposes echarts and restores the server markup", async function (fx) {
    var el = mount(fx, "bar");
    var chart = await ready(el);
    AH.destroy(el);
    T.ok(chart.isDisposed());
    T.eq($(el).children().length, 1);
    T.eq($(el).children()[0].className, "ah-chart-data");
    AH.mount(fx);
    var again = await ready(el);
    T.ok(again !== chart && painted(el) > 1000, "mounts again");
  });

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
