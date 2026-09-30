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
 *   ah:chart-<event>  detail: ChartItemEvent (the item) for click,
 *                     dblclick, mouseover, mouseout, restore;
 *                     ChartLegendEvent ({name, selected}) for
 *                     legendselectchanged; ChartZoomEvent ({start, end,
 *                     startValue, endValue, batch}) for datazoom
 *   ah:chart-ready    detail: ChartReadyEvent ({retheme: boolean}), after
 *                     each draw
 *   ah:chart-data     detail: ChartDataEvent ({text: the server's node as
 *                     HTML, or null}), after setOption / setData changed
 *                     the data
 *
 * Exports (dataText also on AH.lib.chart, for the tests and page
 * scripts): the readable data node of an option (dataText) and its update
 * in a root (updateText).
 */
import AH from "../core.ts";
import type { EChartsCoreOption, EChartsType, Payload } from "echarts";

type EchartsModule = typeof import("echarts");

/** An echarts option as the server writes it (JSON). */
export type ChartOption = Record<string, unknown>;

/** Detail of ah:chart-click, -dblclick, -mouseover, -mouseout, -restore:
 *  echarts' event parameters, as echarts gives them. */
export interface ChartItemEvent {
  componentType: unknown;
  seriesType: unknown;
  seriesIndex: unknown;
  seriesName: unknown;
  name: unknown;
  dataIndex: unknown;
  dataType: unknown;
  value: unknown;
  data: unknown;
}
/** Detail of ah:chart-legendselectchanged. */
export interface ChartLegendEvent { name: unknown; selected: unknown; }
/** Detail of ah:chart-datazoom. */
export interface ChartZoomEvent { start: unknown; end: unknown; startValue: unknown; endValue: unknown; batch: unknown; }
/** Detail of ah:chart-ready. */
export interface ChartReadyEvent { retheme: boolean; }
/** Detail of ah:chart-data: the server's node as HTML, or null. */
export interface ChartDataEvent { text: string | null; }

/** What getDataURL takes (echarts' options). */
export type DataURLOptions = Parameters<EChartsType["getDataURL"]>[0];

/** The echarts theme of a chart (themeOf). */
interface ChartTheme {
  darkMode: boolean;
  color?: string[];
  _loading: { text: string; color: string; textColor: string; maskColor: string };
  [key: string]: unknown;
}

type Rec = Record<string, unknown>;
interface Table { head: string[]; rows: string[][]; }

const EVENTS = ["click", "dblclick", "mouseover", "mouseout",
                "legendselectchanged", "datazoom", "restore"];
const PALETTE = ["primary", "success", "warning", "error", "info", "secondary"];
const TOKEN = /^\s*(?:var\(\s*)?(--ah-[A-Za-z0-9_-]+)\s*(?:,[^)]*)?\)?\s*$/;

// ------------------------------------------------------------------
// Colours
// ------------------------------------------------------------------

function isColor(s: string): boolean {
  return !!(s && window.CSS && CSS.supports && CSS.supports("color", s));
}

/** Any CSS colour as rgb()/rgba(), by painting one pixel with it
 *  (remembered per colour). */
class RgbConverter {
  #ctx: CanvasRenderingContext2D | null = null;
  #cache = new Map<string, string>();

  toRgb(c: string): string {
    const known = this.#cache.get(c);
    if (known !== undefined) { return known; }
    let out = c;
    if (isColor(c) && !/^(transparent|currentcolor|inherit|initial|unset)$/i.test(c)) {
      if (!this.#ctx) {
        const cv = document.createElement("canvas");
        cv.width = cv.height = 1;
        this.#ctx = cv.getContext("2d", { willReadFrequently: true });
      }
      const ctx = this.#ctx;
      if (ctx) {
        ctx.clearRect(0, 0, 1, 1);
        ctx.fillStyle = "#000";
        ctx.fillStyle = c;
        ctx.fillRect(0, 0, 1, 1);
        const p = ctx.getImageData(0, 0, 1, 1).data;
        const r = p[0] as number, g = p[1] as number, b = p[2] as number, a = p[3] as number;
        out = a === 255 ? "rgb(" + r + "," + g + "," + b + ")"
          : "rgba(" + r + "," + g + "," + b + "," + +(a / 255).toFixed(3) + ")";
      }
    }
    this.#cache.set(c, out);
    return out;
  }
}

const rgb = new RgbConverter();

function withAlpha(c: string, a: number): string {
  const m = /^rgba?\((\d+),(\d+),(\d+)/.exec(c || "");
  return m ? "rgba(" + m[1] + "," + m[2] + "," + m[3] + "," + a + ")" : c;
}

function dark(c: string): boolean {
  const m = /^rgba?\((\d+),(\d+),(\d+)/.exec(c || "");
  return !!m && (0.299 * +(m[1] as string) + 0.587 * +(m[2] as string) + 0.114 * +(m[3] as string)) < 128;
}

// Replace every string that is a key of `map` (old colour -> new colour).
function remap(x: unknown, map: Map<string, string>): unknown {
  if (typeof x === "string") { const v = map.get(x); return v === undefined ? x : v; }
  if (Array.isArray(x)) { return x.map((y: unknown) => remap(y, map)); }
  if (x && typeof x === "object" && Object.getPrototypeOf(x) === Object.prototype) {
    const out: Rec = {};
    Object.keys(x).forEach((k) => { out[k] = remap((x as Rec)[k], map); });
    return out;
  }
  return x;
}

// ------------------------------------------------------------------
// Theme changes
// ------------------------------------------------------------------

/** The charts on the page that follow theme changes: on the ah:theme
 *  event or a change of the theme attributes of <html>, each one's
 *  callback runs (once per burst, after a tick). */
class ThemeWatcher {
  #live: (() => void)[] = [];
  #watching = false;
  #pending = false;

  add(f: () => void): void {
    this.#live.push(f);
    this.watch();
  }

  remove(f: () => void): void {
    this.#live = this.#live.filter((x) => x !== f);
  }

  private watch(): void {
    if (this.#watching) { return; }
    this.#watching = true;
    const schedule = (): void => { this.schedule(); };
    document.addEventListener("ah:theme", schedule);
    if (typeof MutationObserver !== "undefined") {
      new MutationObserver(schedule).observe(document.documentElement, {
        attributes: true,
        attributeFilter: ["data-theme", "data-palette", "data-skin", "data-typography",
                          "class", "style"]
      });
    }
  }

  private schedule(): void {
    if (this.#pending) { return; }
    this.#pending = true;
    setTimeout(() => {
      this.#pending = false;
      this.#live.forEach((f) => { f(); });
    }, 0);
  }
}

const themes = new ThemeWatcher();

// ------------------------------------------------------------------
// Readable data (the same rules as aihtml_lib_chart:data_text/2)
// ------------------------------------------------------------------

const TEXT = ":scope > .ah-chart-text";
const MAX_ROWS = 500;
const CARTESIAN = ["line", "bar", "scatter", "effectScatter", "pictorialBar"];

/** The id sequence of readable data nodes made here (a registry). */
const textIds = { seq: 0 };

function isMap(x: unknown): x is Rec { return !!x && typeof x === "object" && !Array.isArray(x); }
function get(k: string, m: unknown): unknown { return isMap(m) ? m[k] : undefined; }
function all(x: unknown): unknown[] { return x === undefined || x === null ? [] : Array.isArray(x) ? x : [x]; }
function first(x: unknown): unknown {
  if (!Array.isArray(x)) { return x; }
  for (const y of x as unknown[]) {
    if (y !== undefined && y !== null) { return y; }
  }
  return undefined;
}
function isScalar(v: unknown): boolean {
  return v === undefined || v === null || typeof v === "number" || typeof v === "string" ||
    typeof v === "boolean";
}
function allMaps(xs: unknown[]): xs is Rec[] { return xs.every(isMap); }

// A scalar as table text ("-" and missing values empty).
function str(v: unknown): string {
  if (typeof v === "string") { return v === "-" ? "" : v; }
  if (typeof v === "number" || typeof v === "boolean") { return String(v); }
  return "";
}

// echarts names an unnamed series "series\0<n>" in getOption.
function seriesName(s: unknown, i: number): string {
  const n = str(get("name", s));
  return n && !/^series\u0000/.test(n) ? n : "Series " + i;
}

function cells(l: unknown): string[] | null {
  if (!Array.isArray(l)) { return null; }
  const vs = (l as unknown[]).map((d) => isMap(d) ? d["value"] : d);
  return vs.every(isScalar) ? vs.map(str) : null;
}

function nth<T>(i: number, l: readonly T[], dflt: T): T { return i >= 1 && i <= l.length ? l[i - 1] as T : dflt; }
function seq(n: number): number[] { const out: number[] = []; for (let i = 1; i <= n; i++) { out.push(i); } return out; }
function nameOf(x: unknown): string { return isMap(x) ? str(get("name", x)) : str(x); }
function nonNull<T>(xs: (T | null)[]): xs is T[] { return xs.every((x) => x !== null); }

function isCategory(a: unknown): boolean {
  if (!isMap(a)) { return false; }
  const t = str(get("type", a));
  return t === "category" || (t === "" && Array.isArray(get("data", a)));
}

function axisName(axis: unknown, dflt: string): string { return str(get("name", first(axis))) || dflt; }

function datasetTable(d: unknown): Table | null {
  const src = get("source", d);
  if (!Array.isArray(src) || !src.length) { return null; }
  const rows = src as unknown[];
  if (Array.isArray(rows[0])) {
    if (!rows.every(Array.isArray)) { return null; }
    const arrays = rows as unknown[][];
    return { head: (arrays[0] as unknown[]).map(str), rows: arrays.slice(1).map((r) => r.map(str)) };
  }
  if (isMap(rows[0])) {
    if (!allMaps(rows)) { return null; }
    let dims = all(get("dimensions", d)).map(nameOf);
    if (!dims.length) {
      dims = Object.keys(rows[0] as Rec).map(str).filter((k, i, a) => a.indexOf(k) === i)
        .sort((a, b) => a < b ? -1 : a > b ? 1 : 0);
    }
    return { head: dims, rows: rows.map((r) => dims.map((k) => str(r[k]))) };
  }
  return null;
}

function axisTable(o: unknown, series: unknown[]): Table | null {
  const axes = [first(get("xAxis", o)), first(get("yAxis", o))].filter(isCategory);
  if (!axes.length) { return xyTable(o, series); }
  const cols = series.map((s) => cells(get("data", s)));
  if (!nonNull(cols)) { return null; }
  const labels = all(get("data", axes[0])).map(str);
  const n = Math.max(labels.length, ...cols.map((c) => c.length));
  return {
    head: [str(get("name", axes[0])) || AH.t("chart_table", "category", "Category")]
      .concat(series.map((s, i) => seriesName(s, i + 1))),
    rows: seq(n).map((i) => [nth(i, labels, String(i))].concat(cols.map((c) => nth(i, c, ""))))
  };
}

function xy(d: unknown): string[] | null {
  if (isMap(d)) { return xy(d["value"]); }
  return Array.isArray(d) && d.length >= 2 ? cells([d[0], d[1]]) : null;
}

function xyTable(o: unknown, series: unknown[]): Table | null {
  const parts = series.map((s) => all(get("data", s)).map(xy));
  const ok: string[][][] = [];
  for (const p of parts) { if (!nonNull(p)) { return null; } ok.push(p); }
  const multi = series.length > 1;
  const rows: string[][] = [];
  ok.forEach((ps, i) => {
    ps.forEach((p) => { rows.push((multi ? [seriesName(series[i], i + 1)] : []).concat(p)); });
  });
  return { head: (multi ? [AH.t("chart_table", "series", "Series")] : []).concat([axisName(get("xAxis", o), "X"),
                                                    axisName(get("yAxis", o), "Y")]),
           rows };
}

function axisPos(v: unknown, labels: string[]): number | null {
  if (typeof v === "number" && Number.isInteger(v) && v >= 0 && v < labels.length) { return v + 1; }
  const i = labels.indexOf(str(v));
  return i < 0 ? null : i + 1;
}

function heatmapTable(o: unknown, s: unknown): Table | null {
  const x = first(get("xAxis", o)), y = first(get("yAxis", o));
  const xs = all(get("data", x)).map(str), ys = all(get("data", y)).map(str);
  if (!isCategory(x) || !isCategory(y) || !xs.length || !ys.length) { return null; }
  const m = new Map<string, string>();
  let ok = true;
  all(get("data", s)).forEach((d) => {
    const v = isMap(d) ? d["value"] : d;
    const i = Array.isArray(v) && v.length >= 3 ? axisPos(v[0], xs) : null;
    const j = i && Array.isArray(v) ? axisPos(v[1], ys) : null;
    const c = j && Array.isArray(v) ? cells([v[2]]) : null;
    if (!c) { ok = false; return; }
    m.set(i + "," + j, c[0] as string);
  });
  if (!ok) { return null; }
  return { head: [axisName(y, AH.t("chart_table", "category", "Category"))].concat(xs),
           rows: ys.map((yl, j) => [yl].concat(xs.map((_, i) => {
             const v = m.get((i + 1) + "," + (j + 1));
             return v === undefined ? "" : v;
           }))) };
}

function pieTable(series: unknown[]): Table | null {
  const parts = series.map((s): string[][] | null => {
    const data = get("data", s);
    if (!Array.isArray(data)) { return null; }
    const items = (data as unknown[]).map((d): [string, unknown] => isMap(d) ? [str(d["name"]), d["value"]] : ["", d]);
    if (!items.every((it) => {
      const v = it[1];
      return typeof v === "number" || v === undefined || v === null || v === "-";
    })) { return null; }
    const total = items.reduce((t, it) => typeof it[1] === "number" ? t + it[1] : t, 0);
    return items.map((it) => {
      const v = it[1];
      return [it[0], str(v),
              typeof v === "number" && total > 0 ? (v * 100 / total).toFixed(1) + "%" : ""];
    });
  });
  if (!nonNull(parts)) { return null; }
  const multi = series.length > 1, rows: string[][] = [];
  parts.forEach((p, i) => {
    p.forEach((r) => { rows.push((multi ? [seriesName(series[i], i + 1)] : []).concat(r)); });
  });
  return { head: (multi ? [AH.t("chart_table", "series", "Series")] : [])
    .concat([AH.t("chart_table", "name", "Name"), AH.t("chart_table", "value", "Value"),
             AH.t("chart_table", "share", "Share")]), rows };
}

function radarTable(o: unknown, series: unknown[]): Table | null {
  const inds = all(get("indicator", first(get("radar", o)))).map(nameOf);
  const items: [string, string[] | null][] = [];
  series.forEach((s) => {
    all(get("data", s)).forEach((d) => {
      items.push(isMap(d) ? [str(d["name"]), cells(d["value"])] : ["", cells(d)]);
    });
  });
  if (!inds.length || items.some((it) => it[1] === null)) { return null; }
  return { head: [AH.t("chart_table", "indicator", "Indicator")]
             .concat(items.map((it, i) => it[0] || AH.t("chart_table", "series_n", "Series {0}", [i + 1]))),
           rows: inds.map((ind, j) => [ind].concat(items.map((it) => nth(j + 1, it[1] || [], "")))) };
}

function graphTable(s: unknown): Table | null {
  const nodes = all(first([get("data", s), get("nodes", s)]));
  const links = all(first([get("links", s), get("edges", s)]));
  const cats = all(get("categories", s)).map(nameOf);
  if (!nodes.length || !allMaps(nodes) || !allMaps(links)) { return null; }
  const ids = nodes.map((n) => str(first([n["id"], n["name"]])));
  const labels = nodes.map((n) => str(n["name"]));
  const names = nodes.map((_, i) => labels[i] || ids[i] || "");
  const index = (ref: unknown): number | null => {
    if (typeof ref === "number" && Number.isInteger(ref) && ref >= 0 && ref < nodes.length) {
      return ref + 1;
    }
    const r = str(ref);
    let i = ids.indexOf(r);
    if (i < 0) { i = labels.indexOf(r); }
    return i < 0 ? null : i + 1;
  };
  const ends = links.map((l) => ({ src: index(l["source"]), tg: index(l["target"]),
                                   raw: str(l["target"]), label: str(l["value"]) }));
  return {
    head: [AH.t("chart_table", "node", "Node")].concat(cats.length ? [AH.t("chart_table", "category", "Category")] : [],
                                   [AH.t("chart_table", "links_to", "Links to")]),
    rows: nodes.map((n, k) => {
      const out = ends.filter((e) => e.src === k + 1).map((e) => {
        const t = e.tg === null ? e.raw : names[e.tg - 1] || "";
        return e.label ? t + " (" + e.label + ")" : t;
      }).join(", ");
      const c = n["category"];
      const cat = typeof c === "number" && Number.isInteger(c) ? nth(c + 1, cats, "") : str(c);
      return [names[k] || ""].concat(cats.length ? [cat] : [], [out]);
    })
  };
}

function treeRows(nodes: unknown[]): string[][] {
  let out: string[][] = [];
  nodes.forEach((n) => {
    if (!isMap(n)) { return; }
    const kids = all(n["children"]).filter(isMap);
    if (str(n["id"]) !== "__root__") {
      out.push([str(n["name"]), kids.map((k) => str(k["name"])).join(", ")]);
    }
    out = out.concat(treeRows(kids));
  });
  return out;
}

function seriesTable(o: unknown, series: unknown[]): Table | null {
  if (!series.length) { return null; }
  const types = series.map((s) => str(get("type", s)))
    .filter((t, i, a) => a.indexOf(t) === i);
  const only = (allowed: string[]): boolean => types.every((t) => allowed.indexOf(t) >= 0);
  if (only(["pie", "funnel"])) { return pieTable(series); }
  if (types.length === 1 && types[0] === "radar") { return radarTable(o, series); }
  if (types.length === 1 && series.length === 1) {
    if (types[0] === "graph") { return graphTable(series[0]); }
    if (types[0] === "tree") {
      const rows = treeRows(all(get("data", series[0])));
      return rows.length
        ? { head: [AH.t("chart_table", "node", "Node"), AH.t("chart_table", "children", "Children")], rows }
        : null;
    }
    if (types[0] === "heatmap") { return heatmapTable(o, series[0]); }
  }
  return only(CARTESIAN) ? axisTable(o, series) : null;
}

function tableOf(o: unknown): Table | null {
  return datasetTable(first(get("dataset", o))) || seriesTable(o, all(get("series", o)));
}

// The option's title (echarts' getOption gives a list of titles).
function titleOf(o: unknown): string | null {
  for (const t of all(get("title", o))) {
    const s = str(get("text", t));
    if (s) { return s; }
  }
  return null;
}

function node(tag: string, text?: string | null, attrs?: Record<string, string>): HTMLElement {
  const n = document.createElement(tag);
  if (text !== null && text !== undefined) { n.textContent = text; }
  const as = attrs || {};
  Object.keys(as).forEach((k) => { n.setAttribute(k, as[k] as string); });
  return n;
}

/** The readable data node of an option, as the server writes it (without
 *  an id): a div with a table captioned `caption', a paragraph with just
 *  the caption when the option's shape has no table, or null. */
export function dataText(option: unknown, caption: string | null | undefined): HTMLElement | null {
  const t = tableOf(option || {});
  const cls = { "class": "ah-chart-text ah-sr-only" };
  if (!t) { return caption ? node("p", caption, cls) : null; }
  const w = Math.max(t.head.length, ...t.rows.map((r) => r.length));
  const pad = (r: string[]): string[] => { const out = r.slice(); while (out.length < w) { out.push(""); } return out; };
  // in a div: a table grows to its content whatever its width
  const box = node("div", null, cls);
  const table = box.appendChild(node("table"));
  if (caption) { table.appendChild(node("caption", caption)); }
  const tr = node("tr");
  pad(t.head).forEach((h) => { tr.appendChild(node("th", h, { scope: "col" })); });
  table.appendChild(node("thead")).appendChild(tr);
  const body = table.appendChild(node("tbody"));
  t.rows.slice(0, MAX_ROWS).forEach((r0) => {
    const r = pad(r0);
    const row = body.appendChild(node("tr"));
    row.appendChild(node("th", r[0], { scope: "row" }));
    r.slice(1).forEach((c) => { row.appendChild(node("td", c)); });
  });
  if (t.rows.length > MAX_ROWS) {
    body.appendChild(node("tr")).appendChild(
      node("td", AH.t("chart_table", "more_rows", "And {0} more rows.", [t.rows.length - MAX_ROWS]),
           { colspan: String(w) }));
  }
  return box;
}

/** Bring the readable data node of `holder' (a chart root, or the root
 *  that owns an aria-hidden chart) up to date: `html' is the server's new
 *  node, or else the node is rebuilt from `option'. As on the server the
 *  caption is the option's title, or else `label' (the holder's
 *  aria-label as the server wrote it). The node keeps its id; a new node
 *  goes after `after' and the holder names it with aria-describedby. */
export function updateText(holder: Element, after: Element | null, option: unknown,
                           html: string | null | undefined, label: string | null): Element | null {
  const old = holder.querySelector(TEXT);
  const cap = label || null;
  let next: Element | null;
  if (typeof html === "string") {
    const tpl = document.createElement("template");
    tpl.innerHTML = html;
    next = tpl.content.firstElementChild;
    const table = next && next.querySelector("table");
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
    next.id = holder.id ? holder.id + "-data" : "ah-chart-text-js" + (++textIds.seq);
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

// ------------------------------------------------------------------
// Chart
// ------------------------------------------------------------------

function readIsland(el: Element): ChartOption {
  const island = el.querySelector(":scope > script.ah-chart-data");
  if (!island) { return {}; }
  try {
    const o: unknown = JSON.parse(island.textContent || "{}");
    return isMap(o) ? o : {};
  } catch (e) { console.error("aihtml: bad chart option", e); return {}; }
}

// The server's nodes, taken out of the root (echarts' init and dispose
// empty the container); put them back first with restore.
function detachServerNodes(el: Element): Element[] {
  const own = Array.from(el.querySelectorAll(
    ":scope > script.ah-chart-data, :scope > .ah-chart-text, :scope > .ah-chart-overlay"));
  own.forEach((n) => { n.remove(); });
  return own;
}

function restore(el: Element, own: Element[]): void {
  el.prepend(...own);
}

function renderable(el: HTMLElement): boolean {
  return el.isConnected && el.offsetWidth > 0 && el.offsetHeight > 0;
}

function ariaOf(el: Element): Rec {
  if (el.querySelector(TEXT)) { return { enabled: true, label: { enabled: false } }; }
  const label = el.getAttribute("aria-label");
  const lab: Rec = { enabled: true };
  if (label) { lab["description"] = label; }
  return { enabled: true, label: lab };
}

// echarts' aria description, unless the option brings its own. With a
// readable data node echarts' label stays off: it would set role="img"
// on the root, which hides the table's cells from screen readers.
function withAria(el: Element, option: ChartOption): ChartOption {
  if (option["aria"] || el.getAttribute("aria-hidden") === "true") { return option; }
  return Object.assign({}, option, { aria: ariaOf(el) });
}

// echarts checks the option itself: the JSON the server wrote, or what a
// method got, goes to it as it is.
function ec(o: ChartOption): EChartsCoreOption { return o as EChartsCoreOption; }

function text(v: unknown): string {
  if (v === null || v === undefined) { return ""; }
  return typeof v === "object" ? JSON.stringify(v) : String(v);
}

class ChartController extends AH.Controller {
  #option: ChartOption = {};
  #chart: EChartsType | null = null;
  #echarts: EchartsModule | null = null;
  #queue: ((chart: EChartsType) => void)[] = [];
  #label: string | null = null;
  /** token -> the value it resolved to (colours as rgb()) */
  #tokens = new Map<string, string>();
  #theme: ChartTheme | null = null;
  #dead = false;
  #loading = false;
  #ro: ResizeObserver | null = null;
  #onTheme = (): void => { this.retheme(); };

  override setup(): void {
    const el = this.element;
    this.#option = readIsland(el);
    this.#chart = null;
    this.#echarts = null;
    this.#queue = [];
    this.#label = el.getAttribute("aria-label");
    this.#tokens = new Map();
    this.#theme = null;
    this.#dead = false;
    this.#loading = el.getAttribute("data-ah-loading") === "true";
    themes.add(this.#onTheme);
    if (typeof ResizeObserver !== "undefined") {
      this.#ro = new ResizeObserver(() => {
        if (this.#dead) { return; }
        if (this.#chart) {
          if (!this.#chart.isDisposed()) { this.#chart.resize(); }
        } else if (this.#echarts && renderable(el)) {
          this.create();
        }
      });
      this.#ro.observe(el);
    }
    import("echarts").then((echarts) => {
      if (this.#dead) { return; }
      this.#echarts = echarts;
      if (renderable(el)) { this.create(); }
    }, (err: unknown) => {
      console.error("aihtml: cannot load echarts", err);
    });
  }

  override teardown(): void {
    const el = this.element;
    this.#dead = true;
    if (this.#ro) { this.#ro.disconnect(); this.#ro = null; }
    themes.remove(this.#onTheme);
    const chart = this.#chart;
    if (chart && !chart.isDisposed()) {
      // dispose empties the container too
      const own = detachServerNodes(el);
      chart.dispose();
      restore(el, own);
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)

  /** `html': the server's readable data node for the new option
   *  (chart_update/3 with a record); without it the node is rebuilt.
   *  notMerge: true replaces the option, a list of component types
   *  replaces those (replaceMerge). */
  setOption(option?: ChartOption | null, notMerge?: boolean | string[] | null, html?: string | null): void {
    const server = typeof html === "string";
    if (server) { this.dataChanged(html); }
    this.withChart((chart) => {
      const o = this.resolveOption(option || {});
      if (notMerge === true) {
        chart.setOption(ec(withAria(this.element, o)), { notMerge: true });
      } else if (Array.isArray(notMerge)) {
        chart.setOption(ec(o), { replaceMerge: notMerge });
      } else {
        chart.setOption(ec(o));
      }
      if (!server) { this.dataChanged(); }
    });
  }
  setData(list?: unknown[] | null): void {
    this.setOption({ series: (list || []).map((d) => ({ data: d })) });
  }
  resize(): void { this.withChart((c) => { c.resize(); }); }
  showLoading(): void {
    this.#loading = true;
    this.withChart(() => { this.paintLoading(); });
  }
  hideLoading(): void {
    this.#loading = false;
    this.withChart((c) => { c.hideLoading(); });
  }
  dispatchAction(action: Payload): void { this.withChart((c) => { c.dispatchAction(action); }); }
  toggleSeries(name: string): void {
    this.withChart((c) => {
      c.dispatchAction({ type: "legendToggleSelect", name });
    });
  }
  /** Redraw with the view (pan, zoom, force layout) back to its start. */
  resetView(): void {
    this.withChart((c) => {
      const o = c.getOption();
      all(o["series"]).forEach((sr) => {
        if (isMap(sr)) { delete sr["center"]; delete sr["zoom"]; }
      });
      c.setOption(o, { notMerge: true });
    });
  }
  getOption(): EChartsCoreOption | null {
    return this.#chart ? this.#chart.getOption() : null;
  }
  /** The echarts instance (null until echarts has loaded and drawn). */
  instance(): EChartsType | null {
    const c = this.#chart;
    return !this.#dead && c && !c.isDisposed() ? c : null;
  }
  getDataURL(opts?: DataURLOptions): string | null {
    if (!this.#chart) { return null; }
    return this.#chart.getDataURL(Object.assign({ type: "png" as const, pixelRatio: 2,
                                                  backgroundColor: this.#tokens.get("--ah-color-bg-paper") || "#fff" },
                                                opts));
  }
  saveAsImage(filename?: string): void {
    const url = this.getDataURL();
    if (!url) { return; }
    const a = document.createElement("a");
    a.href = url;
    a.download = (filename || "chart") + ".png";
    document.body.appendChild(a);
    a.click();
    a.remove();
  }

  // ---- tokens and theme ----------------------------------------------

  // The value of a custom property on the root, colours as rgb();
  // remembered in #tokens so that a theme change can swap it.
  private tokenValue(name: string): string | null {
    let v = getComputedStyle(this.element).getPropertyValue(name).trim();
    if (!v) { return null; }
    v = isColor(v) ? rgb.toRgb(v) : v;
    this.#tokens.set(name, v);
    return v;
  }

  // A copy of `x` with its token strings resolved.
  private resolve(x: unknown): unknown {
    if (typeof x === "string") {
      const m = TOKEN.exec(x);
      if (!m) { return x; }
      const v = this.tokenValue(m[1] as string);
      return v === null ? x : v;
    }
    if (Array.isArray(x)) { return x.map((y: unknown) => this.resolve(y)); }
    if (x && typeof x === "object") {
      const out: Rec = {};
      Object.keys(x).forEach((k) => { out[k] = this.resolve((x as Rec)[k]); });
      return out;
    }
    return x;
  }

  private resolveOption(o: ChartOption): ChartOption {
    return this.resolve(o) as ChartOption;   // a copy of a map is a map
  }

  // The echarts theme of a chart: palette, text, lines and font of the
  // current aihtml theme.
  private themeOf(): ChartTheme {
    const t = (n: string): string | null => this.tokenValue("--ah-" + n);
    const text = t("color-text") || "#333";
    const sec = t("color-text-secondary") || text;
    const muted = t("color-text-muted") || sec;
    const border = t("color-border") || "#ccc";
    const subtle = t("color-border-subtle") || border;
    const paper = t("color-bg-paper") || "#fff";
    const bg = t("color-bg") || paper;
    const off = t("color-text-disabled") || muted;
    const font = t("font-family") || getComputedStyle(this.element).fontFamily;
    const palette = PALETTE.map((n) => t("color-" + n)).filter((c): c is string => !!c);
    const axis = {
      axisLine: { lineStyle: { color: border } },
      axisTick: { lineStyle: { color: border } },
      axisLabel: { color: sec },
      splitLine: { lineStyle: { color: subtle } },
      nameTextStyle: { color: sec }
    };
    const theme: ChartTheme = {
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

  // New echarts theme, and the colours that came from tokens swapped for
  // their new values in the current option (so data set since the first
  // render stays).
  private retheme(): void {
    const chart = this.#chart;
    if (this.#dead || !chart || chart.isDisposed()) { return; }
    const old = this.#tokens;
    this.#tokens = new Map();
    const theme = this.themeOf();
    const map = new Map<string, string>();
    let changed = false;
    old.forEach((was, name) => {
      const known = this.#tokens.get(name);
      const now = known !== undefined ? known : this.tokenValue(name);
      if (now !== null && now !== was) {
        changed = true;
        if (!map.has(was)) { map.set(was, now); }
      }
    });
    old.forEach((was, n) => {
      if (!this.#tokens.has(n)) { this.#tokens.set(n, was); }
    });
    if (!changed) { return; }
    const remapped = remap(chart.getOption(), map);
    const option: ChartOption = isMap(remapped) ? remapped : {};
    option["darkMode"] = theme.darkMode;
    if (typeof chart.setTheme === "function") {
      chart.setTheme(theme);
    }
    this.#theme = theme;
    chart.setOption(ec(option), { notMerge: true });
    if (this.#loading) { this.paintLoading(); }
    this.fire<ChartReadyEvent>("ah:chart-ready", { retheme: true });
  }

  // ---- drawing ---------------------------------------------------------

  private create(): void {
    const el = this.element, echarts = this.#echarts;
    if (!echarts) { return; }
    // echarts measures the container once, at init: never on a 0x0 one
    // (a hidden tab); the ResizeObserver creates it when it gets a size.
    const theme = this.#theme = this.themeOf();
    // init empties the container: keep the server's nodes (first, so that
    // a morph of the root lines them up with the new markup)
    const own = detachServerNodes(el);
    const renderer = el.getAttribute("data-ah-renderer") === "svg" ? "svg" : "canvas";
    // echarts' own texts (toolbox ...) in the page's language, not the
    // browser's: it has English and Chinese built in
    const locale = /^zh\b/i.test(document.documentElement.lang) ? "ZH" : "EN";
    const chart = this.#chart = echarts.init(el, theme, { renderer, locale });
    restore(el, own);
    chart.setOption(ec(withAria(el, this.resolveOption(this.#option))), { notMerge: true });
    EVENTS.forEach((ev) => {
      chart.on(ev, (p: unknown) => { this.bridge(ev, p); });
    });
    if (this.#loading) { this.paintLoading(); }
    const q = this.#queue;
    this.#queue = [];
    q.forEach((f) => { f(chart); });
    this.fire<ChartReadyEvent>("ah:chart-ready", { retheme: false });
  }

  private withChart(f: (chart: EChartsType) => void): void {
    const c = this.#chart;
    if (c && !c.isDisposed()) { f(c); return; }
    this.#queue.push(f);
  }

  private paintLoading(): void {
    if (this.#chart && this.#theme) { this.#chart.showLoading("default", this.#theme._loading); }
  }

  // Bridge an echarts event to a DOM event. Clicks also write the item to
  // data-* attributes of the root, which an action bound to the event
  // receives as Event.data (and the name as Event.value).
  private bridge(ev: string, param: unknown): void {
    const p: Rec = isMap(param) ? param : {};
    let info: ChartItemEvent | ChartLegendEvent | ChartZoomEvent;
    if (ev === "legendselectchanged") {
      info = { name: p["name"], selected: p["selected"] };
    } else if (ev === "datazoom") {
      info = { start: p["start"], end: p["end"], startValue: p["startValue"], endValue: p["endValue"],
               batch: p["batch"] };
    } else {
      info = { componentType: p["componentType"], seriesType: p["seriesType"],
               seriesIndex: p["seriesIndex"], seriesName: p["seriesName"], name: p["name"],
               dataIndex: p["dataIndex"], dataType: p["dataType"], value: p["value"], data: p["data"] };
    }
    if (ev === "click" || ev === "dblclick") {
      const el = this.element;
      el.setAttribute("data-series", text(p["seriesName"]));
      el.setAttribute("data-series-index", text(p["seriesIndex"]));
      el.setAttribute("data-name", text(p["name"]));
      el.setAttribute("data-value", text(p["value"]));
      el.setAttribute("data-index", text(p["dataIndex"]));
      el.setAttribute("data-kind", text(p["dataType"] || p["componentType"]));
      el.setAttribute("data-ah-value", text(p["name"]));
    }
    this.fire("ah:chart-" + ev, info);
  }

  // After setOption / setData: the chart's own node follows its data
  // (unless it is aria-hidden: then its owner, told by ah:chart-data, keeps
  // one). When the node comes or goes, echarts' aria label goes or comes
  // (withAria), and with the node the root is a figure named by the
  // server's label again.
  private dataChanged(html?: string): void {
    const el = this.element;
    if (el.getAttribute("aria-hidden") !== "true") {
      const had = !!el.querySelector(TEXT);
      const server = typeof html === "string";
      const has = !!updateText(el, el.querySelector(":scope > script.ah-chart-data"),
                               server || !this.#chart ? null : this.#chart.getOption(), html, this.#label);
      const chart = this.#chart && !this.#chart.isDisposed() ? this.#chart : null;
      if (has !== had && chart && !isMap(this.#option["aria"])) {
        chart.setOption({ aria: { label: { enabled: !has } } });
      }
      if (has && !had) {
        el.setAttribute("role", "figure");
        if (this.#label) { el.setAttribute("aria-label", this.#label); }
        else { el.removeAttribute("aria-label"); }
      }
    }
    this.fire<ChartDataEvent>("ah:chart-data", { text: typeof html === "string" ? html : null });
  }
}

AH.register("chart", ChartController);

/** What AH.lib.chart holds (the tests and page scripts read dataText). */
export interface ChartLib {
  dataText: typeof dataText;
  updateText: typeof updateText;
}
const lib: ChartLib = { dataText, updateText };
AH.lib["chart"] = lib;
