/* relation_graph: the relation-graph behaviour (relation_graph.js) and
 * its nested chart on markup the server renders. SERVER holds a render
 * of aihtml_relation_graph:relation_graph/3 (circular, id "g1", a detail
 * card for "b"); UPDATE the arguments chart_update/3 sends for another
 * relation_graph record. Regenerate them from Erlang if the markup
 * changes. echarts is loaded by a dynamic import() as on a real page.
 * Events are native; every fixture insertion awaits T.ready. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "graph": "<div class=\"ah-relation-graph\" role=\"group\" aria-roledescription=\"relation graph\" aria-describedby=\"g1-data\" tabindex=\"0\" data-ah=\"relation-graph\" data-ah-value=\"\" data-layout=\"circular\" style=\"height:300px;\" id=\"g1\"><div class=\"ah-relation-graph__canvas ah-chart\" data-ah=\"chart\" aria-hidden=\"true\" style=\"height:100%;\"><script class=\"ah-chart-data\" type=\"application/json\">{\"animationDuration\":400,\"color\":[\"--ah-color-primary\",\"--ah-color-success\",\"--ah-color-warning\",\"--ah-color-error\",\"--ah-color-info\",\"--ah-color-secondary\"],\"series\":[{\"bottom\":40,\"categories\":[],\"data\":[{\"id\":\"a\",\"label\":{\"color\":\"--ah-color-primary-contrast\",\"fontSize\":12,\"overflow\":\"truncate\",\"position\":\"inside\",\"show\":true},\"name\":\"Ann\",\"symbol\":\"circle\",\"symbolSize\":34},{\"id\":\"b\",\"label\":{\"color\":\"--ah-color-primary-contrast\",\"fontSize\":12,\"overflow\":\"truncate\",\"position\":\"inside\",\"show\":true},\"name\":\"Bob\",\"symbol\":\"circle\",\"symbolSize\":34},{\"id\":\"c\",\"label\":{\"color\":\"--ah-color-primary-contrast\",\"fontSize\":12,\"overflow\":\"truncate\",\"position\":\"inside\",\"show\":true},\"name\":\"Cy\",\"symbol\":\"circle\",\"symbolSize\":34}],\"draggable\":true,\"edgeLabel\":{\"color\":\"--ah-color-text-secondary\",\"fontSize\":11,\"formatter\":\"{c}\",\"show\":false},\"emphasis\":{\"focus\":\"adjacency\",\"lineStyle\":{\"width\":3}},\"label\":{\"color\":\"--ah-color-primary-contrast\",\"fontSize\":12,\"overflow\":\"truncate\",\"position\":\"inside\",\"show\":true},\"layout\":\"circular\",\"left\":40,\"lineStyle\":{\"color\":\"--ah-color-border\",\"curveness\":0.08},\"links\":[{\"lineStyle\":{\"color\":\"--ah-color-border\",\"opacity\":0.9,\"type\":\"solid\",\"width\":1.8},\"source\":\"a\",\"target\":\"b\",\"value\":\"\"},{\"lineStyle\":{\"color\":\"--ah-color-border\",\"opacity\":0.9,\"type\":\"solid\",\"width\":1.8},\"source\":\"b\",\"target\":\"c\",\"value\":\"\"}],\"right\":40,\"roam\":true,\"top\":40,\"type\":\"graph\"}],\"tooltip\":{\"trigger\":\"item\"}}</script></div><div class=\"ah-chart-text ah-sr-only\" id=\"g1-data\"><table><thead><tr><th scope=\"col\">Node</th><th scope=\"col\">Links to</th></tr></thead><tbody><tr><th scope=\"row\">Ann</th><td>Bob</td></tr><tr><th scope=\"row\">Bob</th><td>Cy</td></tr><tr><th scope=\"row\">Cy</th><td></td></tr></tbody></table></div><div class=\"ah-relation-graph__toolbar\" role=\"toolbar\"><button class=\"ah-relation-graph__tool\" type=\"button\" data-act=\"fit\" title=\"Fit view\" aria-label=\"Fit view\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M8 3H5a2 2 0 00-2 2v3M16 3h3a2 2 0 012 2v3M8 21H5a2 2 0 01-2-2v-3M16 21h3a2 2 0 002-2v-3\"/></svg></button><button class=\"ah-relation-graph__tool\" type=\"button\" data-act=\"refresh\" title=\"Refresh\" aria-label=\"Refresh\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M21 12a9 9 0 11-2.64-6.36M21 3v6h-6\"/></svg></button></div><div class=\"ah-relation-graph__detail\" data-visible=\"false\"><button class=\"ah-relation-graph__detail-close\" type=\"button\" aria-label=\"Close\"><svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.8\" stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\"><path d=\"M18 6L6 18M6 6l12 12\"/></svg></button><div class=\"ah-relation-graph__detail-body\"><div class=\"ah-relation-graph__detail-item\" data-node=\"b\" hidden>Bob card</div></div></div><div class=\"ah-relation-graph__live\" aria-live=\"polite\" aria-atomic=\"true\"></div></div>"
  };

  // chart_update(Ctx, {id, <<"g1">>}, relation_graph({[{a, <<"Ann">>}, {d, <<"Dee">>}],
  //   [{a, d, <<"knows">>}]}, [circular], [])): setOption's arguments
  var UPDATE = { option: {"series": [{"data": [{"id": "a", "label": {"position": "inside", "color": "--ah-color-primary-contrast", "show": true, "fontSize": 12, "overflow": "truncate"}, "name": "Ann", "symbol": "circle", "symbolSize": 34}, {"id": "d", "label": {"position": "inside", "color": "--ah-color-primary-contrast", "show": true, "fontSize": 12, "overflow": "truncate"}, "name": "Dee", "symbol": "circle", "symbolSize": 34}], "label": {"position": "inside", "color": "--ah-color-primary-contrast", "show": true, "fontSize": 12, "overflow": "truncate"}, "links": [{"value": "knows", "source": "a", "lineStyle": {"type": "solid", "width": 1.8, "color": "--ah-color-border", "opacity": 0.9}, "target": "d"}], "type": "graph", "left": 40, "right": 40, "categories": [], "bottom": 40, "top": 40, "lineStyle": {"color": "--ah-color-border", "curveness": 0.08}, "layout": "circular", "roam": true, "draggable": true, "edgeLabel": {"formatter": "{c}", "color": "--ah-color-text-secondary", "show": true, "fontSize": 11}, "emphasis": {"lineStyle": {"width": 3}, "focus": "adjacency"}}], "color": ["--ah-color-primary", "--ah-color-success", "--ah-color-warning", "--ah-color-error", "--ah-color-info", "--ah-color-secondary"], "tooltip": {"trigger": "item"}, "animationDuration": 400},
                 text: "<div class=\"ah-chart-text ah-sr-only\"><table><thead><tr><th scope=\"col\">Node</th><th scope=\"col\">Links to</th></tr></thead><tbody><tr><th scope=\"row\">Ann</th><td>Dee (knows)</td></tr><tr><th scope=\"row\">Dee</th><td></td></tr></tbody></table></div>" };

  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.firstChild;
  }

  function q(el, sel) { return el.querySelector(sel); }
  function canvasOf(el) { return el.querySelector(":scope > .ah-relation-graph__canvas"); }

  // The rows of the readable data table.
  function rows(el) {
    return Array.from(el.querySelectorAll(":scope > .ah-chart-text tr")).map(function (tr) {
      return Array.from(tr.children).map(function (c) { return c.textContent; });
    });
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
    var el = await mount(fx, "graph");
    var canvas = canvasOf(el);
    var chart = await ready(canvas);
    var sel = [], clicks = [];
    el.addEventListener("ah:select", function (e) { sel.push(e.detail.id); });
    el.addEventListener("ah:node-click", function (e) { clicks.push(e.detail.id); });
    var layout = chart.getModel().getSeriesByIndex(0).getData().getItemLayout(1);
    var p = chart.convertToPixel({ seriesIndex: 0 }, layout);
    clickAt(canvas, p[0], p[1]);
    T.eq(clicks, ["b"]);
    T.eq(sel, ["b"]);
    T.eq(el.getAttribute("data-ah-value"), "b");
    T.eq(q(el, ".ah-relation-graph__detail").getAttribute("data-visible"), "true");
    T.eq(q(el, ".ah-relation-graph__detail-item[data-node=b]").hasAttribute("hidden"), false);
    T.eq(q(el, ".ah-relation-graph__live").textContent, "Bob");
    clickAt(canvas, p[0], p[1]);
    T.eq(sel, ["b"], "same node again");
    q(el, ".ah-relation-graph__detail-close").click();
    T.eq(sel, ["b", null]);
    T.eq(el.getAttribute("data-ah-value"), "");
    T.eq(q(el, ".ah-relation-graph__detail").getAttribute("data-visible"), "false");
  });

  T.test("relation-graph: keyboard walks the nodes, Escape clears", async function (fx) {
    var el = await mount(fx, "graph");
    await ready(canvasOf(el));
    var sel = [];
    el.addEventListener("ah:select", function (e) { sel.push(e.detail.id); });
    function key(k) { return T.key(el, k); }
    el.focus();
    T.ok(!key("ArrowRight"), "default prevented");
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
    T.ok(key("Escape"), "nothing to clear");
  });

  T.test("relation-graph: select / getSelected / fit / refresh", async function (fx) {
    var el = await mount(fx, "graph");
    var canvas = canvasOf(el);
    var chart = await ready(canvas);
    var sel = 0, refresh = 0;
    el.addEventListener("ah:select", function () { sel++; });
    el.addEventListener("ah:refresh", function () { refresh++; });
    AH.invoke(el, "select", "c");
    T.eq(AH.invoke(el, "getSelected"), "c");
    T.eq(sel, 0, "methods do not fire ah:select");
    T.eq(q(el, ".ah-relation-graph__detail").getAttribute("data-visible"), "false", "no card for c");
    q(el, "[data-act=refresh]").click();
    T.eq(refresh, 1);
    q(el, "[data-act=fit]").click();
    T.eq(chart.getOption().series[0].data.length, 3);
    AH.invoke(el, "setOption", { series: [{ data: [{ id: "z", name: "Zed" }], links: [] }] });
    T.eq(chart.getOption().series[0].data.length, 1);
  });
  T.test("relation-graph: focus pans a node into the middle", async function (fx) {
    var el = await mount(fx, "graph");
    var canvas = canvasOf(el);
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

  T.test("relation-graph: removal and re-insertion keep working", async function (fx) {
    var el = await mount(fx, "graph");
    var first = await ready(canvasOf(el));
    el.remove();
    await sleep(0);
    T.ok(first.isDisposed(), "nested chart disposed");
    fx.appendChild(el);
    await T.ready(fx);
    await ready(canvasOf(el));
    var sel = [];
    el.addEventListener("ah:select", function (e) { sel.push(e.detail.id); });
    T.key(el, "End");
    T.eq(sel, ["c"], "one listener after re-insertion");
    T.eq(q(el, ".ah-relation-graph__live").textContent, "Cy");
  });

  T.test("relation-graph: the nodes table stays beside the canvas and follows setOption", async function (fx) {
    var el = await mount(fx, "graph");
    var canvas = canvasOf(el);
    var chart = await ready(canvas);
    var t = el.querySelector(":scope > .ah-chart-text");
    T.eq(el.getAttribute("aria-describedby"), "g1-data");
    T.eq(t.id, "g1-data");
    T.eq(t.previousElementSibling, canvas);
    T.eq(canvas.querySelector(".ah-chart-text"), null, "the aria-hidden canvas has none");
    T.eq(rows(el), [["Node", "Links to"], ["Ann", "Bob"], ["Bob", "Cy"], ["Cy", ""]]);
    // chart_update/3: the server's table
    AH.invoke(el, "setOption", UPDATE.option, true, UPDATE.text);
    T.eq(chart.getOption().series[0].data.length, 2);
    T.eq(rows(el), [["Node", "Links to"], ["Ann", "Dee (knows)"], ["Dee", ""]]);
    T.eq(el.querySelector(":scope > .ah-chart-text").id, "g1-data");
    // a merge in the browser: rebuilt from the canvas's option
    AH.invoke(el, "setOption", { series: [{ links: [{ source: "d", target: "a" }] }] });
    T.eq(rows(el), [["Node", "Links to"], ["Ann", ""], ["Dee", "Ann"]]);
    T.eq(el.querySelectorAll(".ah-chart-text").length, 1);
  });
})(window.AHTest, window.AH);
