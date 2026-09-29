/* The `chart' behaviour, shared by every echarts component (chart,
 * area_chart, bar_chart, donut_chart, radar_chart, and the canvas nested
 * in relation_graph; designs/04-components.md). Ported from sigil
 * (data/chart). The server builds every echarts option (aihtml_chart,
 * aihtml_lib_chart and the convenience chart modules) and writes it as
 * JSON into <script type="application/json" class="ah-chart-data"> inside
 * the root. The behaviour loads echarts (AH.vendor), themes it from the
 * --ah-* custom properties, draws the option, follows size
 * (ResizeObserver) and theme changes, fires ah:chart-click / -dblclick /
 * -mouseover / -mouseout / -legendselectchanged / -datazoom / -restore,
 * and has methods for aihtml_action:call/4 (setOption, setData, resize,
 * showLoading, ...).
 *
 * Theme colours: any string "--ah-color-x" or "var(--ah-color-x)" in an
 * option is replaced by that custom property's value on the chart, as an
 * rgba() colour (echarts cannot read var() or every CSS colour syntax).
 * On a theme change (the ah:theme event or a change of the theme
 * attributes on <html>) each chart gets a new echarts theme and every
 * colour that came from a token is swapped for its new value.
 *
 * AH.lib.chart: the chart state of an element (state), token resolution
 * (resolve) and the echarts theme of a chart (themeOf).
 */
import $ from "jquery";
import AH from "../core.js";

var EVENTS = ["click", "dblclick", "mouseover", "mouseout",
              "legendselectchanged", "datazoom", "restore"];
var PALETTE = ["primary", "success", "warning", "error", "info", "secondary"];
var TOKEN = /^\s*(?:var\(\s*)?(--ah-[A-Za-z0-9_-]+)\s*(?:,[^)]*)?\)?\s*$/;

// ------------------------------------------------------------------
// Colours
// ------------------------------------------------------------------

var ctx2d = null;
var rgbCache = {};

function isColor(s) {
  return !!(s && window.CSS && CSS.supports && CSS.supports("color", s));
}

// Any CSS colour as rgb()/rgba(), by painting one pixel with it.
function toRgb(c) {
  if (rgbCache.hasOwnProperty(c)) { return rgbCache[c]; }
  var out = c;
  if (isColor(c) && !/^(transparent|currentcolor|inherit|initial|unset)$/i.test(c)) {
    if (!ctx2d) {
      var cv = document.createElement("canvas");
      cv.width = cv.height = 1;
      ctx2d = cv.getContext("2d", { willReadFrequently: true });
    }
    if (ctx2d) {
      ctx2d.clearRect(0, 0, 1, 1);
      ctx2d.fillStyle = "#000";
      ctx2d.fillStyle = c;
      ctx2d.fillRect(0, 0, 1, 1);
      var p = ctx2d.getImageData(0, 0, 1, 1).data;
      out = p[3] === 255 ? "rgb(" + p[0] + "," + p[1] + "," + p[2] + ")"
        : "rgba(" + p[0] + "," + p[1] + "," + p[2] + "," + +(p[3] / 255).toFixed(3) + ")";
    }
  }
  rgbCache[c] = out;
  return out;
}

function withAlpha(rgb, a) {
  var m = /^rgba?\((\d+),(\d+),(\d+)/.exec(rgb || "");
  return m ? "rgba(" + m[1] + "," + m[2] + "," + m[3] + "," + a + ")" : rgb;
}

function dark(rgb) {
  var m = /^rgba?\((\d+),(\d+),(\d+)/.exec(rgb || "");
  return !!m && (0.299 * m[1] + 0.587 * m[2] + 0.114 * m[3]) < 128;
}

// The value of a custom property on `el`, colours as rgb(); remembered
// in s.tokens so that a theme change can swap it.
function tokenValue(s, name) {
  var v = getComputedStyle(s.el).getPropertyValue(name).trim();
  if (!v) { return null; }
  v = isColor(v) ? toRgb(v) : v;
  s.tokens[name] = v;
  return v;
}

// A copy of `x` with its token strings resolved.
function resolve(s, x) {
  if (typeof x === "string") {
    var m = TOKEN.exec(x);
    if (!m) { return x; }
    var v = tokenValue(s, m[1]);
    return v === null ? x : v;
  }
  if (Array.isArray(x)) { return x.map(function (y) { return resolve(s, y); }); }
  if (x && typeof x === "object") {
    var out = {};
    Object.keys(x).forEach(function (k) { out[k] = resolve(s, x[k]); });
    return out;
  }
  return x;
}

// Replace every string that is a key of `map` (old colour -> new colour).
function remap(x, map) {
  if (typeof x === "string") { return map.hasOwnProperty(x) ? map[x] : x; }
  if (Array.isArray(x)) { return x.map(function (y) { return remap(y, map); }); }
  if (x && typeof x === "object" && Object.getPrototypeOf(x) === Object.prototype) {
    var out = {};
    Object.keys(x).forEach(function (k) { out[k] = remap(x[k], map); });
    return out;
  }
  return x;
}

// The echarts theme of a chart: palette, text, lines and font of the
// current aihtml theme.
function themeOf(s) {
  function t(n) { return tokenValue(s, "--ah-" + n); }
  var text = t("color-text") || "#333";
  var sec = t("color-text-secondary") || text;
  var muted = t("color-text-muted") || sec;
  var border = t("color-border") || "#ccc";
  var subtle = t("color-border-subtle") || border;
  var paper = t("color-bg-paper") || "#fff";
  var bg = t("color-bg") || paper;
  var off = t("color-text-disabled") || muted;
  var font = t("font-family") || getComputedStyle(s.el).fontFamily;
  var palette = PALETTE.map(function (n) { return t("color-" + n); }).filter(Boolean);
  var axis = {
    axisLine: { lineStyle: { color: border } },
    axisTick: { lineStyle: { color: border } },
    axisLabel: { color: sec },
    splitLine: { lineStyle: { color: subtle } },
    nameTextStyle: { color: sec }
  };
  var theme = {
    darkMode: dark(bg),
    backgroundColor: "transparent",
    // no global text colour: echarts picks contrasting colours for
    // labels inside shapes by itself
    textStyle: { fontFamily: font },
    title: { textStyle: { color: text }, subtextStyle: { color: muted } },
    legend: { textStyle: { color: sec }, inactiveColor: off,
              pageTextStyle: { color: sec }, pageIconColor: sec, pageIconInactiveColor: off },
    tooltip: { backgroundColor: paper, borderColor: border, confine: true,
               textStyle: { color: text, fontFamily: font } },
    axisPointer: { lineStyle: { color: muted }, crossStyle: { color: muted },
                   label: { backgroundColor: sec, color: paper } },
    categoryAxis: axis, valueAxis: axis, logAxis: axis, timeAxis: axis,
    dataZoom: { textStyle: { color: sec }, borderColor: border },
    visualMap: { textStyle: { color: sec } },
    toolbox: { iconStyle: { borderColor: sec } },
    graph: { lineStyle: { color: border } },
    _loading: { text: "", color: palette[0] || text, textColor: text,
                maskColor: withAlpha(bg, 0.7) }
  };
  if (palette.length) { theme.color = palette; }
  return theme;
}

// ------------------------------------------------------------------
// Theme changes
// ------------------------------------------------------------------

var live = [];
var watching = false;
var pending = false;

function watchTheme() {
  if (watching) { return; }
  watching = true;
  $(document).on("ah:theme.ahcharts", schedule);
  if (window.MutationObserver) {
    new MutationObserver(schedule).observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["data-theme", "data-palette", "data-skin", "data-typography",
                        "class", "style"]
    });
  }
}

function schedule() {
  if (pending) { return; }
  pending = true;
  setTimeout(function () {
    pending = false;
    live.forEach(function (el) { retheme(state(el)); });
  }, 0);
}

// New echarts theme, and the colours that came from tokens swapped for
// their new values in the current option (so data set since the first
// render stays).
function retheme(s) {
  if (!s || !s.chart || s.chart.isDisposed()) { return; }
  var old = s.tokens;
  s.tokens = {};
  var theme = themeOf(s);
  var map = {}, changed = false;
  Object.keys(old).forEach(function (name) {
    var now = s.tokens.hasOwnProperty(name) ? s.tokens[name] : tokenValue(s, name);
    if (now !== null && now !== old[name]) {
      changed = true;
      if (!map.hasOwnProperty(old[name])) { map[old[name]] = now; }
    }
  });
  Object.keys(old).forEach(function (n) {
    if (!s.tokens.hasOwnProperty(n)) { s.tokens[n] = old[n]; }
  });
  if (!changed) { return; }
  var option = remap(s.chart.getOption(), map);
  option.darkMode = theme.darkMode;
  if (typeof s.chart.setTheme === "function") {
    s.chart.setTheme(theme);
  }
  s.theme = theme;
  s.chart.setOption(option, { notMerge: true });
  if (s.loading) { showLoading(s); }
  $(s.el).trigger("ah:chart-ready", [{ retheme: true }]);
}

// ------------------------------------------------------------------
// Chart
// ------------------------------------------------------------------

function state(el) { return $.data(el, "ah-chart"); }

function readIsland(el) {
  var node = $(el).children("script.ah-chart-data")[0];
  if (!node) { return {}; }
  try { return JSON.parse(node.textContent || "{}"); }
  catch (e) { console.error("aihtml: bad chart option", e); return {}; }
}

function serverNodes(el) {
  return $(el).children("script.ah-chart-data, .ah-chart-overlay");
}

function renderable(el) {
  return el.isConnected && el.offsetWidth > 0 && el.offsetHeight > 0;
}

// echarts' aria description, unless the option brings its own.
function withAria(el, option) {
  if (option.aria || el.getAttribute("aria-hidden") === "true") { return option; }
  var label = el.getAttribute("aria-label");
  var aria = { enabled: true };
  if (label) { aria.label = { description: label }; }
  return $.extend({}, option, { aria: aria });
}

function create(s) {
  var el = s.el;
  // echarts measures the container once, at init: never on a 0x0 one
  // (a hidden tab); the ResizeObserver creates it when it gets a size.
  s.theme = themeOf(s);
  // init empties the container: keep the server's nodes (first, so that
  // a morph of the root lines them up with the new markup)
  var own = serverNodes(el).detach();
  s.chart = s.echarts.init(el, s.theme,
                           { renderer: el.getAttribute("data-ah-renderer") || "canvas" });
  $(el).prepend(own);
  s.chart.setOption(withAria(el, resolve(s, s.option)), { notMerge: true });
  EVENTS.forEach(function (ev) {
    s.chart.on(ev, function (p) { fire(s, ev, p); });
  });
  if (s.loading) { showLoading(s); }
  var q = s.queue;
  s.queue = [];
  q.forEach(function (f) { f(s.chart); });
  $(el).trigger("ah:chart-ready", [{ retheme: false }]);
}

function withChart(s, f) {
  if (s.chart && !s.chart.isDisposed()) { return f(s.chart); }
  s.queue.push(f);
  return undefined;
}

function showLoading(s) {
  s.chart.showLoading("default", s.theme._loading);
}

function text(v) {
  if (v === null || v === undefined) { return ""; }
  return typeof v === "object" ? JSON.stringify(v) : String(v);
}

// Bridge an echarts event to jQuery. Clicks also write the item to data-*
// attributes of the root, which an action bound to the event receives as
// Event.data (and the name as Event.value).
function fire(s, ev, p) {
  var info;
  if (ev === "legendselectchanged") {
    info = { name: p.name, selected: p.selected };
  } else if (ev === "datazoom") {
    info = { start: p.start, end: p.end, startValue: p.startValue, endValue: p.endValue,
             batch: p.batch };
  } else {
    info = { componentType: p.componentType, seriesType: p.seriesType,
             seriesIndex: p.seriesIndex, seriesName: p.seriesName, name: p.name,
             dataIndex: p.dataIndex, dataType: p.dataType, value: p.value, data: p.data };
  }
  if (ev === "click" || ev === "dblclick") {
    var el = s.el;
    el.setAttribute("data-series", text(p.seriesName));
    el.setAttribute("data-series-index", text(p.seriesIndex));
    el.setAttribute("data-name", text(p.name));
    el.setAttribute("data-value", text(p.value));
    el.setAttribute("data-index", text(p.dataIndex));
    el.setAttribute("data-kind", text(p.dataType || p.componentType));
    el.setAttribute("data-ah-value", text(p.name));
  }
  $(s.el).trigger("ah:chart-" + ev, [info]);
}

function setOption(s, option, notMerge) {
  return withChart(s, function (chart) {
    var o = resolve(s, option || {});
    if (notMerge === true) {
      chart.setOption(withAria(s.el, o), { notMerge: true });
    } else if (Array.isArray(notMerge)) {
      chart.setOption(o, { replaceMerge: notMerge });
    } else {
      chart.setOption(o);
    }
  });
}

AH.define("chart", {
  init: function (el) {
    var s = { el: el, option: readIsland(el), chart: null, echarts: null, queue: [],
              tokens: {}, theme: null, dead: false,
              loading: el.getAttribute("data-ah-loading") === "true" };
    $.data(el, "ah-chart", s);
    live.push(el);
    watchTheme();
    if (window.ResizeObserver) {
      s.ro = new ResizeObserver(function () {
        if (s.dead) { return; }
        if (s.chart) {
          if (!s.chart.isDisposed()) { s.chart.resize(); }
        } else if (s.echarts && renderable(el)) {
          create(s);
        }
      });
      s.ro.observe(el);
    }
    AH.vendor("echarts").then(function (echarts) {
      if (s.dead) { return; }
      s.echarts = echarts;
      if (renderable(el)) { create(s); }
    }, function (err) {
      console.error("aihtml: cannot load echarts", err);
    });
  },
  destroy: function (el) {
    var s = state(el);
    if (!s) { return; }
    s.dead = true;
    if (s.ro) { s.ro.disconnect(); }
    live = live.filter(function (x) { return x !== el; });
    if (s.chart && !s.chart.isDisposed()) {
      // dispose empties the container too
      var own = serverNodes(el).detach();
      s.chart.dispose();
      $(el).prepend(own);
    }
    $.removeData(el, "ah-chart");
  },
  methods: {
    setOption: function (el, $el, option, notMerge) { setOption(state(el), option, notMerge); },
    setData: function (el, $el, list) {
      setOption(state(el), { series: (list || []).map(function (d) { return { data: d }; }) });
    },
    resize: function (el) {
      withChart(state(el), function (c) { c.resize(); });
    },
    showLoading: function (el) {
      var s = state(el);
      s.loading = true;
      withChart(s, function () { showLoading(s); });
    },
    hideLoading: function (el) {
      var s = state(el);
      s.loading = false;
      withChart(s, function (c) { c.hideLoading(); });
    },
    dispatchAction: function (el, $el, action) {
      withChart(state(el), function (c) { c.dispatchAction(action); });
    },
    toggleSeries: function (el, $el, name) {
      withChart(state(el), function (c) {
        c.dispatchAction({ type: "legendToggleSelect", name: name });
      });
    },
    // Redraw with the view (pan, zoom, force layout) back to its start.
    resetView: function (el) {
      withChart(state(el), function (c) {
        var o = c.getOption();
        (o.series || []).forEach(function (sr) { delete sr.center; delete sr.zoom; });
        c.setOption(o, { notMerge: true });
      });
    },
    getOption: function (el) {
      var s = state(el);
      return s && s.chart ? s.chart.getOption() : null;
    },
    // The echarts instance (null until echarts has loaded and drawn).
    instance: function (el) {
      var s = state(el);
      return s && s.chart && !s.chart.isDisposed() ? s.chart : null;
    },
    getDataURL: function (el, $el, opts) {
      var s = state(el);
      if (!s || !s.chart) { return null; }
      return s.chart.getDataURL($.extend({ type: "png", pixelRatio: 2,
                                           backgroundColor: s.tokens["--ah-color-bg-paper"] || "#fff" },
                                         opts));
    },
    saveAsImage: function (el, $el, filename) {
      var url = AH.invoke(el, "getDataURL");
      if (!url) { return; }
      var a = document.createElement("a");
      a.href = url;
      a.download = (filename || "chart") + ".png";
      document.body.appendChild(a);
      a.click();
      a.remove();
    }
  }
});

AH.lib = AH.lib || {};
AH.lib.chart = { state: state, resolve: resolve, themeOf: themeOf };
