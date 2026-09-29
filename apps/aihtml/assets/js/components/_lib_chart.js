/* The `chart' behaviour, shared by every echarts component (chart,
 * area_chart, bar_chart, donut_chart, radar_chart, and the canvas nested
 * in relation_graph; designs/04-components.md). Ported from sigil
 * (data/chart). The server builds every echarts option (aihtml_chart,
 * aihtml_lib_chart and the convenience chart modules) and writes it as
 * JSON into <script type="application/json" class="ah-chart-data"> inside
 * the root. The behaviour loads echarts (a dynamic import(): Vite puts it
 * in its own lazily loaded chunk), themes it from the --ah-* custom
 * properties, draws the option, follows size (ResizeObserver) and theme
 * changes, fires ah:chart-click / -dblclick / -mouseover / -mouseout /
 * -legendselectchanged / -datazoom / -restore, and has methods for
 * aihtml_action:call/4 (setOption, setData, resize, showLoading, ...).
 *
 * Readable data: the server also writes the chart's data as a table in a
 * visually hidden <div class="ah-chart-text ah-sr-only"> (or just a
 * caption, in a <p> of those classes), which the root names with aria-describedby
 * (aihtml_lib_chart:data_text/2). echarts' init and dispose empty the
 * root, so that node is kept like the data island. When the data
 * changes, the node follows: setOption with a third argument (the
 * server's new node as HTML, which chart_update/3 sends for a record)
 * puts that in its place; any other setOption or setData rebuilds it
 * from echarts' merged option with dataText, the same rules as the
 * server's. An aria-hidden chart (the canvas of relation_graph) has no
 * node of its own: its owner keeps one, on ah:chart-data.
 *
 * Theme colours: any string "--ah-color-x" or "var(--ah-color-x)" in an
 * option is replaced by that custom property's value on the chart, as an
 * rgba() colour (echarts cannot read var() or every CSS colour syntax).
 * On a theme change (the ah:theme event or a change of the theme
 * attributes on <html>) each chart gets a new echarts theme and every
 * colour that came from a token is swapped for its new value.
 *
 * Events (native CustomEvents on the root, bubbling):
 *   ah:chart-<event>  detail: the item ({componentType, seriesType,
 *                     seriesIndex, seriesName, name, dataIndex, dataType,
 *                     value, data}; {name, selected} for
 *                     legendselectchanged; {start, end, startValue,
 *                     endValue, batch} for datazoom)
 *   ah:chart-ready    detail: {retheme: boolean}, after each draw
 *   ah:chart-data     detail: {text: the server's node as HTML, or null},
 *                     after setOption / setData changed the data
 *
 * AH.lib.chart (and exports): the chart state of an element (state),
 * token resolution (resolve), the echarts theme of a chart (themeOf), the
 * readable data node of an option (dataText) and its update in a root
 * (updateText).
 */
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
  document.addEventListener("ah:theme", schedule);
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

function emit(el, type, detail) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
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
  emit(s.el, "ah:chart-ready", { retheme: true });
}

// ------------------------------------------------------------------
// Readable data (the same rules as aihtml_lib_chart:data_text/2)
// ------------------------------------------------------------------

var TEXT = ":scope > .ah-chart-text";
var MAX_ROWS = 500;
var CARTESIAN = ["line", "bar", "scatter", "effectScatter", "pictorialBar"];
var textSeq = 0;

function isMap(x) { return !!x && typeof x === "object" && !Array.isArray(x); }
function get(k, m) { return isMap(m) ? m[k] : undefined; }
function all(x) { return x === undefined || x === null ? [] : Array.isArray(x) ? x : [x]; }
function first(x) {
  if (!Array.isArray(x)) { return x; }
  for (var i = 0; i < x.length; i++) {
    if (x[i] !== undefined && x[i] !== null) { return x[i]; }
  }
  return undefined;
}
function isScalar(v) {
  return v === undefined || v === null || typeof v === "number" || typeof v === "string" ||
    typeof v === "boolean";
}

// A scalar as table text ("-" and missing values empty).
function str(v) {
  if (typeof v === "string") { return v === "-" ? "" : v; }
  if (typeof v === "number" || typeof v === "boolean") { return String(v); }
  return "";
}

// echarts names an unnamed series "series\0<n>" in getOption.
function seriesName(s, i) {
  var n = str(get("name", s));
  return n && !/^series\u0000/.test(n) ? n : "Series " + i;
}

function cells(l) {
  if (!Array.isArray(l)) { return null; }
  var vs = l.map(function (d) { return isMap(d) ? d.value : d; });
  return vs.every(isScalar) ? vs.map(str) : null;
}

function nth(i, l, dflt) { return i >= 1 && i <= l.length ? l[i - 1] : dflt; }
function seq(n) { var out = []; for (var i = 1; i <= n; i++) { out.push(i); } return out; }
function nameOf(x) { return isMap(x) ? str(get("name", x)) : str(x); }

function isCategory(a) {
  if (!isMap(a)) { return false; }
  var t = str(get("type", a));
  return t === "category" || (t === "" && Array.isArray(get("data", a)));
}

function axisName(axis, dflt) { return str(get("name", first(axis))) || dflt; }

function datasetTable(d) {
  var src = get("source", d);
  if (!Array.isArray(src) || !src.length) { return null; }
  if (Array.isArray(src[0])) {
    if (!src.every(Array.isArray)) { return null; }
    return { head: src[0].map(str), rows: src.slice(1).map(function (r) { return r.map(str); }) };
  }
  if (isMap(src[0])) {
    if (!src.every(isMap)) { return null; }
    var dims = all(get("dimensions", d)).map(nameOf);
    if (!dims.length) {
      dims = Object.keys(src[0]).map(str).filter(function (k, i, a) { return a.indexOf(k) === i; })
        .sort(function (a, b) { return a < b ? -1 : a > b ? 1 : 0; });
    }
    return { head: dims, rows: src.map(function (r) {
      return dims.map(function (k) { return str(r[k]); });
    }) };
  }
  return null;
}

function axisTable(o, series) {
  var axes = [first(get("xAxis", o)), first(get("yAxis", o))].filter(isCategory);
  if (!axes.length) { return xyTable(o, series); }
  var cols = series.map(function (s) { return cells(get("data", s)); });
  if (cols.indexOf(null) >= 0) { return null; }
  var labels = all(get("data", axes[0])).map(str);
  var n = Math.max.apply(null, [labels.length].concat(cols.map(function (c) { return c.length; })));
  return {
    head: [str(get("name", axes[0])) || "Category"]
      .concat(series.map(function (s, i) { return seriesName(s, i + 1); })),
    rows: seq(n).map(function (i) {
      return [nth(i, labels, String(i))].concat(cols.map(function (c) { return nth(i, c, ""); }));
    })
  };
}

function xy(d) {
  if (isMap(d)) { return xy(d.value); }
  return Array.isArray(d) && d.length >= 2 ? cells([d[0], d[1]]) : null;
}

function xyTable(o, series) {
  var parts = series.map(function (s) { return all(get("data", s)).map(xy); });
  if (parts.some(function (p) { return p.indexOf(null) >= 0; })) { return null; }
  var multi = series.length > 1;
  var rows = [];
  parts.forEach(function (ps, i) {
    ps.forEach(function (p) { rows.push((multi ? [seriesName(series[i], i + 1)] : []).concat(p)); });
  });
  return { head: (multi ? ["Series"] : []).concat([axisName(get("xAxis", o), "X"),
                                                    axisName(get("yAxis", o), "Y")]),
           rows: rows };
}

function axisPos(v, labels) {
  if (typeof v === "number" && Number.isInteger(v) && v >= 0 && v < labels.length) { return v + 1; }
  var i = labels.indexOf(str(v));
  return i < 0 ? null : i + 1;
}

function heatmapTable(o, s) {
  var x = first(get("xAxis", o)), y = first(get("yAxis", o));
  var xs = all(get("data", x)).map(str), ys = all(get("data", y)).map(str);
  if (!isCategory(x) || !isCategory(y) || !xs.length || !ys.length) { return null; }
  var m = {}, ok = true;
  all(get("data", s)).forEach(function (d) {
    var v = isMap(d) ? d.value : d;
    var i = Array.isArray(v) && v.length >= 3 ? axisPos(v[0], xs) : null;
    var j = i ? axisPos(v[1], ys) : null;
    var c = j ? cells([v[2]]) : null;
    if (!c) { ok = false; return; }
    m[i + "," + j] = c[0];
  });
  if (!ok) { return null; }
  return { head: [axisName(y, "Category")].concat(xs),
           rows: ys.map(function (yl, j) {
             return [yl].concat(xs.map(function (_, i) {
               var k = (i + 1) + "," + (j + 1);
               return m.hasOwnProperty(k) ? m[k] : "";
             }));
           }) };
}

function pieTable(series) {
  var parts = series.map(function (s) {
    var data = get("data", s);
    if (!Array.isArray(data)) { return null; }
    var items = data.map(function (d) {
      return isMap(d) ? [str(d.name), d.value] : ["", d];
    });
    if (!items.every(function (it) {
      var v = it[1];
      return typeof v === "number" || v === undefined || v === null || v === "-";
    })) { return null; }
    var total = items.reduce(function (t, it) {
      return typeof it[1] === "number" ? t + it[1] : t;
    }, 0);
    return items.map(function (it) {
      var v = it[1];
      return [it[0], str(v),
              typeof v === "number" && total > 0 ? (v * 100 / total).toFixed(1) + "%" : ""];
    });
  });
  if (parts.indexOf(null) >= 0) { return null; }
  var multi = series.length > 1, rows = [];
  parts.forEach(function (p, i) {
    p.forEach(function (r) { rows.push((multi ? [seriesName(series[i], i + 1)] : []).concat(r)); });
  });
  return { head: (multi ? ["Series"] : []).concat(["Name", "Value", "Share"]), rows: rows };
}

function radarTable(o, series) {
  var inds = all(get("indicator", first(get("radar", o)))).map(nameOf);
  var items = [];
  series.forEach(function (s) {
    all(get("data", s)).forEach(function (d) {
      items.push(isMap(d) ? [str(d.name), cells(d.value)] : ["", cells(d)]);
    });
  });
  if (!inds.length || items.some(function (it) { return it[1] === null; })) { return null; }
  return { head: ["Indicator"].concat(items.map(function (it, i) {
             return it[0] || "Series " + (i + 1);
           })),
           rows: inds.map(function (ind, j) {
             return [ind].concat(items.map(function (it) { return nth(j + 1, it[1], ""); }));
           }) };
}

function graphTable(s) {
  var nodes = all(first([get("data", s), get("nodes", s)]));
  var links = all(first([get("links", s), get("edges", s)]));
  var cats = all(get("categories", s)).map(nameOf);
  if (!nodes.length || !nodes.concat(links).every(isMap)) { return null; }
  var ids = nodes.map(function (n) { return str(first([n.id, n.name])); });
  var labels = nodes.map(function (n) { return str(n.name); });
  var names = nodes.map(function (n, i) { return labels[i] || ids[i]; });
  function index(ref) {
    if (typeof ref === "number" && Number.isInteger(ref) && ref >= 0 && ref < nodes.length) {
      return ref + 1;
    }
    var r = str(ref), i = ids.indexOf(r);
    if (i < 0) { i = labels.indexOf(r); }
    return i < 0 ? null : i + 1;
  }
  var ends = links.map(function (l) {
    return { src: index(l.source), tg: index(l.target), raw: str(l.target), label: str(l.value) };
  });
  return {
    head: ["Node"].concat(cats.length ? ["Category"] : [], ["Links to"]),
    rows: nodes.map(function (n, k) {
      var out = ends.filter(function (e) { return e.src === k + 1; }).map(function (e) {
        var t = e.tg === null ? e.raw : names[e.tg - 1];
        return e.label ? t + " (" + e.label + ")" : t;
      }).join(", ");
      var c = n.category;
      var cat = typeof c === "number" && Number.isInteger(c) ? nth(c + 1, cats, "") : str(c);
      return [names[k]].concat(cats.length ? [cat] : [], [out]);
    })
  };
}

function treeRows(nodes) {
  var out = [];
  nodes.forEach(function (n) {
    if (!isMap(n)) { return; }
    var kids = all(n.children).filter(isMap);
    if (str(n.id) !== "__root__") {
      out.push([str(n.name), kids.map(function (k) { return str(k.name); }).join(", ")]);
    }
    out = out.concat(treeRows(kids));
  });
  return out;
}

function seriesTable(o, series) {
  if (!series.length) { return null; }
  var types = series.map(function (s) { return str(get("type", s)); })
    .filter(function (t, i, a) { return a.indexOf(t) === i; });
  function only(allowed) { return types.every(function (t) { return allowed.indexOf(t) >= 0; }); }
  if (only(["pie", "funnel"])) { return pieTable(series); }
  if (types.length === 1 && types[0] === "radar") { return radarTable(o, series); }
  if (types.length === 1 && series.length === 1) {
    if (types[0] === "graph") { return graphTable(series[0]); }
    if (types[0] === "tree") {
      var rows = treeRows(all(get("data", series[0])));
      return rows.length ? { head: ["Node", "Children"], rows: rows } : null;
    }
    if (types[0] === "heatmap") { return heatmapTable(o, series[0]); }
  }
  return only(CARTESIAN) ? axisTable(o, series) : null;
}

function tableOf(o) {
  return datasetTable(first(get("dataset", o))) || seriesTable(o, all(get("series", o)));
}

// The option's title (echarts' getOption gives a list of titles).
function titleOf(o) {
  var ts = all(get("title", o));
  for (var i = 0; i < ts.length; i++) {
    var t = str(get("text", ts[i]));
    if (t) { return t; }
  }
  return null;
}

function node(tag, text, attrs) {
  var n = document.createElement(tag);
  if (text !== null && text !== undefined) { n.textContent = text; }
  Object.keys(attrs || {}).forEach(function (k) { n.setAttribute(k, attrs[k]); });
  return n;
}

// The readable data node of an option, as the server writes it (without
// an id): a div with a table captioned `caption', a paragraph with just
// the caption when the option's shape has no table, or null.
export function dataText(option, caption) {
  var t = tableOf(option || {});
  var cls = { "class": "ah-chart-text ah-sr-only" };
  if (!t) { return caption ? node("p", caption, cls) : null; }
  var w = Math.max.apply(null, [t.head.length].concat(t.rows.map(function (r) { return r.length; })));
  function pad(r) { r = r.slice(); while (r.length < w) { r.push(""); } return r; }
  // in a div: a table grows to its content whatever its width
  var box = node("div", null, cls);
  var table = box.appendChild(node("table"));
  if (caption) { table.appendChild(node("caption", caption)); }
  var tr = node("tr");
  pad(t.head).forEach(function (h) { tr.appendChild(node("th", h, { scope: "col" })); });
  table.appendChild(node("thead")).appendChild(tr);
  var body = table.appendChild(node("tbody"));
  t.rows.slice(0, MAX_ROWS).forEach(function (r) {
    r = pad(r);
    var row = body.appendChild(node("tr"));
    row.appendChild(node("th", r[0], { scope: "row" }));
    r.slice(1).forEach(function (c) { row.appendChild(node("td", c)); });
  });
  if (t.rows.length > MAX_ROWS) {
    body.appendChild(node("tr")).appendChild(
      node("td", "And " + (t.rows.length - MAX_ROWS) + " more rows.", { colspan: String(w) }));
  }
  return box;
}

// Bring the readable data node of `holder' (a chart root, or the root
// that owns an aria-hidden chart) up to date: `html' is the server's new
// node, or else the node is rebuilt from `option'. As on the server the
// caption is the option's title, or else `label' (the holder's aria-label
// as the server wrote it). The node keeps its id; a new node goes after
// `after' and the holder names it with aria-describedby.
export function updateText(holder, after, option, html, label) {
  var old = holder.querySelector(TEXT);
  var cap = label || null;
  var next;
  if (typeof html === "string") {
    var tpl = document.createElement("template");
    tpl.innerHTML = html;
    next = tpl.content.firstElementChild;
    var table = next && next.querySelector("table");
    if (table && cap && !table.querySelector(":scope > caption")) {
      table.prepend(node("caption", cap));
    } else if (!next && cap) {
      next = dataText(null, cap);
    }
  } else {
    next = dataText(option, titleOf(option) || cap);
  }
  if (old && next) {
    next.id = old.id;
    old.replaceWith(next);
  } else if (next) {
    next.id = holder.id ? holder.id + "-data" : "ah-chart-text-js" + (++textSeq);
    if (after && after.parentNode === holder) { after.after(next); } else { holder.prepend(next); }
    if (!holder.getAttribute("aria-describedby")) {
      holder.setAttribute("aria-describedby", next.id);
    }
  } else if (old) {
    old.remove();
    if (holder.getAttribute("aria-describedby") === old.id) {
      holder.removeAttribute("aria-describedby");
    }
  }
  return next;
}

// After setOption / setData: the chart's own node follows its data
// (unless it is aria-hidden: then its owner, told by ah:chart-data, keeps
// one). When the node comes or goes, echarts' aria label goes or comes
// (withAria), and with the node the root is a figure named by the
// server's label again.
function dataChanged(s, html) {
  var el = s.el;
  if (el.getAttribute("aria-hidden") !== "true") {
    var had = !!el.querySelector(TEXT);
    var server = typeof html === "string";
    var has = !!updateText(el, el.querySelector(":scope > script.ah-chart-data"),
                           server ? null : s.chart.getOption(), html, s.label);
    var chart = s.chart && !s.chart.isDisposed() ? s.chart : null;
    if (has !== had && chart && !isMap(s.option.aria)) {
      chart.setOption({ aria: { label: { enabled: !has } } });
    }
    if (has && !had) {
      el.setAttribute("role", "figure");
      if (s.label) { el.setAttribute("aria-label", s.label); }
      else { el.removeAttribute("aria-label"); }
    }
  }
  emit(el, "ah:chart-data", { text: typeof html === "string" ? html : null });
}

// ------------------------------------------------------------------
// Chart
// ------------------------------------------------------------------

var states = new WeakMap();

function state(el) { return states.get(el); }

function readIsland(el) {
  var node = el.querySelector(":scope > script.ah-chart-data");
  if (!node) { return {}; }
  try { return JSON.parse(node.textContent || "{}"); }
  catch (e) { console.error("aihtml: bad chart option", e); return {}; }
}

// The server's nodes, taken out of the root (echarts' init and dispose
// empty the container); put them back first with restore.
function detachServerNodes(el) {
  var own = Array.from(el.querySelectorAll(
    ":scope > script.ah-chart-data, :scope > .ah-chart-text, :scope > .ah-chart-overlay"));
  own.forEach(function (n) { n.remove(); });
  return own;
}

function restore(el, own) {
  el.prepend.apply(el, own);
}

function renderable(el) {
  return el.isConnected && el.offsetWidth > 0 && el.offsetHeight > 0;
}

// echarts' aria description, unless the option brings its own. With a
// readable data node echarts' label stays off: it would set role="img"
// on the root, which hides the table's cells from screen readers.
function withAria(el, option) {
  if (option.aria || el.getAttribute("aria-hidden") === "true") { return option; }
  return Object.assign({}, option, { aria: ariaOf(el) });
}

function ariaOf(el) {
  if (el.querySelector(TEXT)) { return { enabled: true, label: { enabled: false } }; }
  var label = el.getAttribute("aria-label");
  var aria = { enabled: true, label: { enabled: true } };
  if (label) { aria.label.description = label; }
  return aria;
}

function create(s) {
  var el = s.el;
  // echarts measures the container once, at init: never on a 0x0 one
  // (a hidden tab); the ResizeObserver creates it when it gets a size.
  s.theme = themeOf(s);
  // init empties the container: keep the server's nodes (first, so that
  // a morph of the root lines them up with the new markup)
  var own = detachServerNodes(el);
  s.chart = s.echarts.init(el, s.theme,
                           { renderer: el.getAttribute("data-ah-renderer") || "canvas" });
  restore(el, own);
  s.chart.setOption(withAria(el, resolve(s, s.option)), { notMerge: true });
  EVENTS.forEach(function (ev) {
    s.chart.on(ev, function (p) { fire(s, ev, p); });
  });
  if (s.loading) { showLoading(s); }
  var q = s.queue;
  s.queue = [];
  q.forEach(function (f) { f(s.chart); });
  emit(el, "ah:chart-ready", { retheme: false });
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

// Bridge an echarts event to a DOM event. Clicks also write the item to
// data-* attributes of the root, which an action bound to the event
// receives as Event.data (and the name as Event.value).
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
  emit(s.el, "ah:chart-" + ev, info);
}

// `html': the server's readable data node for the new option
// (chart_update/3 with a record); without it the node is rebuilt.
function setOption(s, option, notMerge, html) {
  var server = typeof html === "string";
  if (server) { dataChanged(s, html); }
  return withChart(s, function (chart) {
    var o = resolve(s, option || {});
    if (notMerge === true) {
      chart.setOption(withAria(s.el, o), { notMerge: true });
    } else if (Array.isArray(notMerge)) {
      chart.setOption(o, { replaceMerge: notMerge });
    } else {
      chart.setOption(o);
    }
    if (!server) { dataChanged(s); }
  });
}

AH.register("chart", class extends AH.Controller {
  setup() {
    var el = this.element;
    var s = { el: el, option: readIsland(el), chart: null, echarts: null, queue: [],
              label: el.getAttribute("aria-label"),
              tokens: {}, theme: null, dead: false,
              loading: el.getAttribute("data-ah-loading") === "true" };
    this.s = s;
    states.set(el, s);
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
    import("echarts").then(function (echarts) {
      if (s.dead) { return; }
      s.echarts = echarts;
      if (renderable(el)) { create(s); }
    }, function (err) {
      console.error("aihtml: cannot load echarts", err);
    });
  }

  teardown() {
    var el = this.element, s = this.s;
    s.dead = true;
    if (s.ro) { s.ro.disconnect(); }
    live = live.filter(function (x) { return x !== el; });
    if (s.chart && !s.chart.isDisposed()) {
      // dispose empties the container too
      var own = detachServerNodes(el);
      s.chart.dispose();
      restore(el, own);
    }
    if (states.get(el) === s) { states.delete(el); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setOption(option, notMerge, html) { setOption(this.s, option, notMerge, html); }
  setData(list) {
    setOption(this.s, { series: (list || []).map(function (d) { return { data: d }; }) });
  }
  resize() { withChart(this.s, function (c) { c.resize(); }); }
  showLoading() {
    var s = this.s;
    s.loading = true;
    withChart(s, function () { showLoading(s); });
  }
  hideLoading() {
    this.s.loading = false;
    withChart(this.s, function (c) { c.hideLoading(); });
  }
  dispatchAction(action) { withChart(this.s, function (c) { c.dispatchAction(action); }); }
  toggleSeries(name) {
    withChart(this.s, function (c) {
      c.dispatchAction({ type: "legendToggleSelect", name: name });
    });
  }
  // Redraw with the view (pan, zoom, force layout) back to its start.
  resetView() {
    withChart(this.s, function (c) {
      var o = c.getOption();
      (o.series || []).forEach(function (sr) { delete sr.center; delete sr.zoom; });
      c.setOption(o, { notMerge: true });
    });
  }
  getOption() {
    var s = this.s;
    return s && s.chart ? s.chart.getOption() : null;
  }
  // The echarts instance (null until echarts has loaded and drawn).
  instance() {
    var s = this.s;
    return s && !s.dead && s.chart && !s.chart.isDisposed() ? s.chart : null;
  }
  getDataURL(opts) {
    var s = this.s;
    if (!s || !s.chart) { return null; }
    return s.chart.getDataURL(Object.assign({ type: "png", pixelRatio: 2,
                                              backgroundColor: s.tokens["--ah-color-bg-paper"] || "#fff" },
                                            opts));
  }
  saveAsImage(filename) {
    var url = this.getDataURL();
    if (!url) { return; }
    var a = document.createElement("a");
    a.href = url;
    a.download = (filename || "chart") + ".png";
    document.body.appendChild(a);
    a.click();
    a.remove();
  }
});

AH.lib = AH.lib || {};
AH.lib.chart = { state: state, resolve: resolve, themeOf: themeOf, dataText: dataText,
                 updateText: updateText };
