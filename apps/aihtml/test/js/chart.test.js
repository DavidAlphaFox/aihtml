/* chart: the chart behaviour (_lib_chart.js) on markup the server
 * renders. SERVER holds renders of aihtml_bar_chart:bar_chart/3 (id
 * "c1") and aihtml_chart:chart/3 with a "var(--ah-color-primary)" colour
 * (id "c2"); regenerate them from Erlang if the markup changes. echarts
 * is loaded through AH.vendor as on a real page. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "bar": "<div class=\"ah-chart\" role=\"img\" data-ah=\"chart\" style=\"height:240px;\" id=\"c1\" aria-label=\"Bars\"><script class=\"ah-chart-data\" type=\"application/json\">{\"xAxis\":{\"data\":[\"a\",\"b\",\"c\"],\"type\":\"category\",\"axisLine\":{\"show\":true},\"axisTick\":{\"show\":false}},\"yAxis\":{\"type\":\"value\",\"axisLine\":{\"show\":false},\"axisTick\":{\"show\":false},\"splitLine\":{\"lineStyle\":{\"type\":\"dashed\"},\"show\":true}},\"series\":[{\"data\":[5,9,4],\"name\":\"Q1\",\"type\":\"bar\",\"itemStyle\":{\"borderRadius\":[4,4,0,0]},\"barMaxWidth\":40}],\"tooltip\":{\"axisPointer\":{\"type\":\"shadow\"},\"trigger\":\"axis\"},\"grid\":{\"left\":12,\"right\":20,\"bottom\":12,\"top\":16,\"containLabel\":true}}</script></div>",
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
})(window.AHTest, window.jQuery, window.AH);
