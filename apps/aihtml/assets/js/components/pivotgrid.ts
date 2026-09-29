/* Pivot table controller (aihtml_pivotgrid), ported from sigil's
 * data/pivotgrid. The server renders the first view; this file re-renders
 * on every view change:
 *
 *   local mode   the engine below aggregates the rows of the root's JSON
 *                data island (script.ah-pg-data) with the same algorithm
 *                as aihtml_pivotgrid.erl and renders the shared templates
 *                AH.tpl.pivotgrid_grid / pivotgrid_fields, so the tables
 *                are byte for byte what the server renders;
 *   remote mode  (data-ah-remote) the new view is written to the root
 *                (data-ah-value, data-view) and 'ah:view' is fired; the
 *                action bound to it answers with pivotgrid_rows/3, which
 *                morphs the server-rendered tables in and calls viewLoaded.
 *
 * Everything else (selection, keyboard, context menu, field list,
 * column resizing, export) reads the rendered DOM, so it works the same
 * in both modes.
 *
 * Events (native CustomEvents, bubbling; the detail in e.detail):
 *   change          the layout changed (data-ah-value)
 *   ah:view         {layout, view}                        (ViewEvent)
 *   ah:selection    [{row, col, vi, value}] (the selected cells; SelectionEvent)
 *   ah:cell-click   {row, col, filter, field, agg, value, text}, also in
 *                   the root's data-cell (JSON) while it is dispatched
 *                   (CellClickEvent) */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import "virtual:ah-tpl/pivotgrid_fields";
import "virtual:ah-tpl/pivotgrid_grid";

/** A member of a row or column field: numbers, texts, blanks. */
export type Key = number | string | null;
/** The members from the outermost field in. */
export type Path = Key[];

/** A measure: an aggregate of a field (null: count the records). */
export interface ValueSpec { field: string | null; agg: string; label: string | null; }

/** The layout (data-ah-value): the fields on each axis and the measures. */
export interface Layout { rows: string[]; columns: string[]; values: ValueSpec[]; }

/** The sort of the row members: by key, or by the values of a column. */
export interface RowSort { by: "key" | "value" | string; dir: string; col?: Path; vi?: number; }

/** The view (data-view): expansions and sorting. */
export interface View {
  expanded_rows?: Path[] | "all";
  expanded_cols?: Path[] | "all";
  row_sort?: RowSort | null;
  col_sort?: string;
}

/** Detail of ah:view. */
export interface ViewEvent { layout: Layout; view: View; }
/** A selected cell. */
export interface CellRef { row: Path; col: Path; vi: number; value: number | null; }
/** Detail of ah:selection. */
export type SelectionEvent = CellRef[];
/** Detail of ah:cell-click (also the root's data-cell while it runs). */
export interface CellClickEvent {
  row: Path; col: Path; filter: Record<string, Key>; field: string | null; agg: string;
  value: number | null; text: string;
}

/** Options of exportXlsx / exportCsv. */
export interface ExportOptions { sheetName?: string; filename?: string; download?: boolean; separator?: string; }

/** What exportData returns: the table as rows of cells and its merged cells. */
export interface ExportTable {
  aoa: (string | number)[][];
  merges: { s: { r: number; c: number }; e: { r: number; c: number } }[];
}

/** A number format (aihtml_pivotgrid:format/1). */
interface NumFormat { decimals: number | null; thousands: string; decimal: string; prefix: string; suffix: string; }

interface FieldDef { name: string; label: string; agg: string; format: NumFormat | null; }

/** The root's data-config. */
interface Config {
  fields: FieldDef[];
  row_subtotals: boolean;
  col_subtotals: boolean;
  grand_totals: boolean;
  values_on_rows: boolean;
  format: NumFormat;
  labels: Record<string, string>;
}

/** What the engine and the views work on. */
interface Model {
  id: string;
  fields: FieldDef[];
  fieldNames: string[];
  layout: Layout;
  view: View;
  row_subtotals: boolean;
  col_subtotals: boolean;
  grand_totals: boolean;
  values_on_rows: boolean;
  format: NumFormat;
  labels: Record<string, string>;
}

/** Accumulator of a measure: [sum, numbers, count, min, max, product]. */
type Acc = [number, number, number, number | null, number | null, number];

interface Engine {
  accs: Map<string, Acc[]>;
  rkids: Map<string, Map<string, Key>>;
  ckids: Map<string, Map<string, Key>>;
}

interface Entry { path: Path; key: Key; depth: number; kids: boolean; exp: boolean; grand?: undefined; }
type RowEntry = Entry | { grand: true };

type SortFun = (parent: Path, ks: Key[]) => Key[];

interface Leaf { kind: "member" | "subtotal" | "grand"; agg: Path; }

/** A column header cell while the column tree is walked. */
interface HeadCell {
  cls: string; colspan: string; rowspan: string; path: string; label: string;
  toggle: string; expanded: string; lvl: number; leaf?: Leaf;
}

interface SortAttrs { sort: string; ci: string; sort_icon: string; aria_sort: string; }

/** The members, measure and value of a body cell. */
interface CellInfo {
  row: Path; col: Path; vi: number; r: number; c: number; filter: Record<string, Key>;
  field: string | null; agg: string; value: number | null; text: string;
}

type RC = [number, number];

interface OpenMenu { ctx: string; target: HTMLElement; invoker: Element | null; }

/** A layout as given (setLayout, the field list): values may be names. */
interface LayoutInput {
  rows?: string[];
  columns?: string[];
  values?: (string | { field?: string | null; agg?: string; label?: string | null })[];
}

// ------------------------------------------------------------------
// Parsing the root's JSON
// ------------------------------------------------------------------

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}
function str(v: unknown, d: string): string { return typeof v === "string" ? v : d; }
function strings(v: unknown): string[] { return Array.isArray(v) ? v.map(String) : []; }

function parseJson(el: Element, attr: string): unknown {
  try { return JSON.parse(el.getAttribute(attr) || ""); } catch (e) { return undefined; }
}

function parseFormat(v: unknown): NumFormat {
  const f = isRecord(v) ? v : {};
  return { decimals: typeof f.decimals === "number" ? f.decimals : null,
           thousands: str(f.thousands, ""), decimal: str(f.decimal, "."),
           prefix: str(f.prefix, ""), suffix: str(f.suffix, "") };
}

function parseConfig(v: unknown): Config {
  const c = isRecord(v) ? v : {};
  const labels: Record<string, string> = {};
  if (isRecord(c.labels)) {
    const l = c.labels;
    Object.keys(l).forEach((k) => { labels[k] = String(l[k]); });
  }
  return {
    fields: (Array.isArray(c.fields) ? c.fields : []).filter(isRecord).map((f) => ({
      name: String(f.name), label: str(f.label, String(f.name)), agg: str(f.agg, "sum"),
      format: isRecord(f.format) ? parseFormat(f.format) : null
    })),
    row_subtotals: c.row_subtotals === true, col_subtotals: c.col_subtotals === true,
    grand_totals: c.grand_totals === true, values_on_rows: c.values_on_rows === true,
    format: parseFormat(c.format), labels
  };
}

function toValueSpec(v: unknown): ValueSpec {
  const o = isRecord(v) ? v : {};
  return { field: typeof o.field === "string" ? o.field : null, agg: str(o.agg, "count"),
           label: typeof o.label === "string" ? o.label : null };
}

function parseLayout(v: unknown): Layout {
  const o = isRecord(v) ? v : {};
  return { rows: strings(o.rows), columns: strings(o.columns),
           values: (Array.isArray(o.values) ? o.values : []).map(toValueSpec) };
}

function toPath(v: unknown): Path { return Array.isArray(v) ? v.map(cell) : []; }

function toPaths(v: unknown): Path[] | "all" | undefined {
  if (v === "all") { return "all"; }
  return Array.isArray(v) ? v.map(toPath) : undefined;
}

function toRowSort(v: unknown): RowSort | null {
  if (!isRecord(v)) { return null; }
  const s: RowSort = { by: str(v.by, "key"), dir: str(v.dir, "asc") };
  if (v.col !== undefined) { s.col = toPath(v.col); }
  if (v.vi !== undefined) { s.vi = Number(v.vi); }
  return s;
}

// The view as the root holds it: its keys are kept as they come (they
// are written back), the ones read here are checked.
function parseView(v: unknown): View | null {
  if (!isRecord(v)) { return null; }
  const out: View & Record<string, unknown> = { ...v };
  if ("expanded_rows" in v) { out.expanded_rows = toPaths(v.expanded_rows); }
  if ("expanded_cols" in v) { out.expanded_cols = toPaths(v.expanded_cols); }
  if ("row_sort" in v) { out.row_sort = toRowSort(v.row_sort); }
  if ("col_sort" in v) { out.col_sort = str(v.col_sort, "asc"); }
  return out;
}

function toLayoutInput(v: unknown): LayoutInput {
  const o = isRecord(v) ? v : {};
  return {
    rows: strings(o.rows), columns: strings(o.columns),
    values: (Array.isArray(o.values) ? o.values : []).map((x: unknown) => {
      if (typeof x === "string") { return x; }
      const r = isRecord(x) ? x : {};
      return { field: typeof r.field === "string" ? r.field : (r.field === null ? null : undefined),
               agg: typeof r.agg === "string" ? r.agg : undefined,
               label: typeof r.label === "string" ? r.label : (r.label === null ? null : undefined) };
    })
  };
}

function toOptions(v: unknown): ExportOptions {
  const o = isRecord(v) ? v : {};
  const out: ExportOptions = {};
  if (typeof o.sheetName === "string") { out.sheetName = o.sheetName; }
  if (typeof o.filename === "string") { out.filename = o.filename; }
  if (typeof o.separator === "string") { out.separator = o.separator; }
  if (o.download === false) { out.download = false; }
  return out;
}

// ------------------------------------------------------------------
// Engine: the same steps as the engine of aihtml_pivotgrid.erl
// ------------------------------------------------------------------

const ROW: unique symbol = Symbol("row");   // the "count the record" marker of a measure without field

function key(p: unknown): string { return JSON.stringify(p); }

// numbers, then texts by code point (Erlang's binary order), then blanks
function rank(k: Key): number { return typeof k === "number" ? 0 : k === null ? 2 : 1; }

function cmpStr(a: string, b: string): number {
  if (!/[\uD800-\uDFFF]/.test(a + b)) { return a < b ? -1 : a > b ? 1 : 0; }
  let i = 0, j = 0;
  while (i < a.length && j < b.length) {
    const ca = a.codePointAt(i) as number, cb = b.codePointAt(j) as number;
    if (ca !== cb) { return ca < cb ? -1 : 1; }
    i += ca > 0xffff ? 2 : 1;
    j += cb > 0xffff ? 2 : 1;
  }
  return (a.length - i) - (b.length - j) < 0 ? -1 : (a.length - i) === (b.length - j) ? 0 : 1;
}

function kcmp(a: Key, b: Key): number {
  const ra = rank(a), rb = rank(b);
  if (ra !== rb) { return ra < rb ? -1 : 1; }
  if (ra === 0) { return (a as number) < (b as number) ? -1 : (a as number) > (b as number) ? 1 : 0; }
  if (ra === 1) { return cmpStr(a as string, b as string); }
  return 0;
}

// a value as the server normalises it
function cell(v: unknown): Key {
  if (v === null || v === undefined) { return null; }
  if (typeof v === "number") { return isFinite(v) ? v : null; }
  if (typeof v === "boolean") { return v ? "true" : "false"; }
  return String(v);
}

function effValues(M: Model): ValueSpec[] {
  return M.layout.values.length ? M.layout.values
    : [{ field: null, agg: "count", label: null }];
}

function prefixes(k: Path): string[] {
  const out: string[] = [];
  for (let n = 0; n <= k.length; n++) { out.push(key(k.slice(0, n))); }
  return out;
}

function addKids(k: Path, map: Map<string, Map<string, Key>>): void {
  for (let n = 0; n < k.length; n++) {
    const pk = key(k.slice(0, n));
    let set = map.get(pk);
    if (!set) { set = new Map(); map.set(pk, set); }
    set.set(key(k[n]), k[n]);
  }
}

function add(a: Acc, v: Key | typeof ROW): void {
  if (v === ROW) { a[2]++; return; }
  if (v === null || v === undefined) { return; }
  if (typeof v === "number") {
    a[0] = a[0] + v; a[1]++; a[2]++;
    if (a[3] === null || v < a[3]) { a[3] = v; }
    if (a[4] === null || v > a[4]) { a[4] = v; }
    a[5] = a[5] * v;
    return;
  }
  a[2]++;
}

function result(agg: string, a: Acc): number | null {
  if (agg === "count" && a[2] > 0) { return a[2]; }
  if (a[1] === 0) { return null; }
  switch (agg) {
    case "sum": return a[0];
    case "avg": return a[0] / a[1];
    case "min": return a[3];
    case "max": return a[4];
    case "product": return a[5];
    default: return null;
  }
}

function aggregate(M: Model, data: readonly Key[][], vs: readonly ValueSpec[]): Engine {
  const idx = new Map<string, number>();
  M.fieldNames.forEach((n, i) => { idx.set(n, i); });
  const ri = M.layout.rows.map((f) => idx.get(f));
  const ci = M.layout.columns.map((f) => idx.get(f));
  const vi = vs.map((v) => v.field === null ? -1 : idx.get(v.field));
  const accs = new Map<string, Acc[]>(), rk = new Map<string, Map<string, Key>>(), ck = new Map<string, Map<string, Key>>();
  const at = (t: readonly Key[], i: number | undefined): Key => {
    const v = i === undefined ? undefined : t[i];
    return v === undefined ? null : v;
  };
  data.forEach((t) => {
    const rkey = ri.map((i) => at(t, i));
    const ckey = ci.map((i) => at(t, i));
    const vals = vi.map((i) => i !== undefined && i < 0 ? ROW : at(t, i));
    const rps = prefixes(rkey), cps = prefixes(ckey);
    for (let a = 0; a < rps.length; a++) {
      for (let b = 0; b < cps.length; b++) {
        const k = rps[a] + "\u0001" + cps[b];
        let acc = accs.get(k);
        if (!acc) {
          acc = vals.map((): Acc => [0, 0, 0, null, null, 1]);
          accs.set(k, acc);
        }
        for (let n = 0; n < vals.length; n++) { add(acc[n], vals[n]); }
      }
    }
    addKids(rkey, rk);
    addKids(ckey, ck);
  });
  return { accs, rkids: rk, ckids: ck };
}

function valueAt(E: Engine, vs: readonly ValueSpec[], rp: Path, cp: Path, v: number): number | null {
  const acc = E.accs.get(key(rp) + "\u0001" + key(cp));
  return acc ? result(vs[v].agg, acc[v]) : null;
}

function keyCmp(dir: string | undefined): (a: Key, b: Key) => number {
  return dir === "desc" ? (a, b) => kcmp(b, a) : kcmp;
}

function valueCmp(E: Engine, vs: readonly ValueSpec[], parent: Path, s: RowSort): (a: Key, b: Key) => number {
  const v = Math.min(s.vi || 0, vs.length - 1), col = s.col || [];
  return (a, b) => {
    const va = valueAt(E, vs, parent.concat([a]), col, v);
    const vb = valueAt(E, vs, parent.concat([b]), col, v);
    if (va === null && vb === null) { return kcmp(a, b); }
    if (va === null) { return 1; }
    if (vb === null) { return -1; }
    if (va < vb) { return s.dir === "asc" ? -1 : 1; }
    if (va > vb) { return s.dir === "asc" ? 1 : -1; }
    return kcmp(a, b);
  };
}

function kidsOf(kids: Map<string, Map<string, Key>>, parent: Path): Key[] | null {
  const set = kids.get(key(parent));
  return set ? Array.from(set.values()) : null;
}

function flatten(kids: Map<string, Map<string, Key>>, exp: Set<string>, sortFun: SortFun, parent: Path,
                 depth: number, out: Entry[]): Entry[] {
  const ks = kidsOf(kids, parent);
  if (!ks) { return out; }
  sortFun(parent, ks).forEach((k) => {
    const p = parent.concat([k]), pk = key(p);
    const has = kids.has(pk), open = has && exp.has(pk);
    out.push({ path: p, key: k, depth, kids: has, exp: open });
    if (open) { flatten(kids, exp, sortFun, p, depth + 1, out); }
  });
  return out;
}

function expandedSet(paths: Path[] | "all" | undefined, kids: Map<string, unknown>): Set<string> {
  const s = new Set<string>();
  if (paths === "all") {
    kids.forEach((_, k) => { if (k !== "[]") { s.add(k); } });
  } else {
    (paths || []).forEach((p) => { s.add(key(p)); });
  }
  return s;
}

function span(n: number): string { return n <= 1 ? "" : String(n); }
function tot(b: boolean | undefined): string { return b ? " ah-pg-total" : ""; }
function leafCls(l: Leaf): string {
  return l.kind === "grand" ? " ah-pg-grand-total" : l.kind === "subtotal" ? " ah-pg-total" : "";
}

function fieldOf(M: Model, name: string): FieldDef | null {
  for (let i = 0; i < M.fields.length; i++) {
    if (M.fields[i].name === name) { return M.fields[i]; }
  }
  return null;
}

function fieldLabel(M: Model, f: string): string {
  const d = fieldOf(M, f);
  return d ? d.label : f;
}

function valueLabel(M: Model, v: ValueSpec): string {
  if (v.label !== null && v.label !== undefined) { return v.label; }
  if (v.field === null) { return M.labels.count; }
  return fieldLabel(M, v.field) + " (" + M.labels[v.agg] + ")";
}

function keyLabel(M: Model, k: Key): string {
  return k === null ? M.labels.blank : typeof k === "number" ? String(k) : k;
}

function raw(v: number | null): string {
  if (v === null) { return ""; }
  return String(v);
}

function thousands(int: string, sep: string): string {
  if (!sep || int.length <= 3) { return int; }
  const first = int.length % 3 || 3;
  let out = int.slice(0, first);
  for (let i = first; i < int.length; i += 3) { out += sep + int.slice(i, i + 3); }
  return out;
}

function formatNumber(v: number, f: NumFormat): string {
  const a = Math.abs(v);
  let s: string;
  if (f.decimals === null || f.decimals === undefined) {
    s = a === Math.trunc(a) ? a.toFixed(0) : a.toFixed(2);
  } else {
    s = a.toFixed(f.decimals);
  }
  const parts = s.split(".");
  return f.prefix + (v < 0 ? "-" : "") + thousands(parts[0], f.thousands) +
    (parts.length > 1 ? f.decimal + parts[1] : "") + f.suffix;
}

function formatValue(M: Model, v: number | null, spec: ValueSpec): string {
  if (v === null) { return ""; }
  let fmt: NumFormat;
  if (spec.agg === "count") {
    fmt = { ...M.format, decimals: null, prefix: "", suffix: "" };
  } else {
    const f = spec.field === null ? null : fieldOf(M, spec.field);
    fmt = (f && f.format) || M.format;
  }
  return formatNumber(v, fmt);
}

// The column tree below parent: its leaves and header cells (tagged
// with their level and, for a leaf cell, the leaf).
function colWalk(M: Model, E: Engine, exp: Set<string>, sortFun: SortFun, parent: Path, level: number,
                 D: number, vcEff: number): { leaves: Leaf[]; cells: HeadCell[] } {
  const leaves: Leaf[] = [], cells: HeadCell[] = [];
  sortFun(parent, kidsOf(E.ckids, parent) || []).forEach((k) => {
    const p = parent.concat([k]), pk = key(p);
    const has = E.ckids.has(pk), open = has && exp.has(pk);
    const label = keyLabel(M, k);
    const base = {
      path: key(p), label, lvl: level,
      toggle: !has ? "" : open ? "ah-pg-toggle ah-pg-toggle-open" : "ah-pg-toggle ah-pg-toggle-closed",
      expanded: !has ? "" : open ? "true" : "false"
    };
    if (open) {
      const sub = colWalk(M, E, exp, sortFun, p, level + 1, D, vcEff);
      const st: Leaf = { kind: "subtotal", agg: p };
      const ls = sub.leaves.concat(M.col_subtotals ? [st] : []);
      cells.push({ ...base, cls: "ah-pg-col-th", colspan: span(ls.length * vcEff), rowspan: "" });
      cells.push(...sub.cells);
      if (M.col_subtotals) {
        cells.push({ cls: "ah-pg-col-th ah-pg-total", colspan: span(vcEff), rowspan: span(D - level),
                     path: "", toggle: "", expanded: "", lvl: level + 1, leaf: st,
                     label: label + " " + M.labels.subtotal });
      }
      leaves.push(...ls);
    } else {
      const leaf: Leaf = { kind: "member", agg: p };
      cells.push({ ...base, cls: "ah-pg-col-th", colspan: span(vcEff), rowspan: span(D - level + 1), leaf });
      leaves.push(leaf);
    }
  });
  return { leaves, cells };
}

function noSort(): SortAttrs { return { sort: "", ci: "", sort_icon: "", aria_sort: "" }; }

function gridView(M: Model, vs: readonly ValueSpec[], E: Engine, rowList: readonly Entry[],
                  colList: readonly Entry[], cexp: Set<string>, csort: SortFun): object {
  const L = M.labels, vc = vs.length;
  const vor = M.values_on_rows && vc > 1, vcEff = vor ? 1 : vc;
  const gt = M.grand_totals, hasCols = M.layout.columns.length > 0;
  const hasRows = M.layout.rows.length > 0, grand = L.grand_total;
  const rowSort = M.view.row_sort;
  const D = colList.reduce((m, c) => Math.max(m, c.path.length), 0);
  const walk = hasCols ? colWalk(M, E, cexp, csort, [], 1, D, vcEff) : { leaves: [], cells: [] };
  const leaves: Leaf[] = walk.leaves.concat(gt || !hasCols ? [{ kind: "grand", agg: [] }] : []);
  const phys: [Leaf, number][] = [];
  leaves.forEach((l) => { for (let v = 0; v < vcEff; v++) { phys.push([l, v]); } });
  const sortAttrs = (leaf: Leaf, v: number, ci: number): SortAttrs => {
    const dir = rowSort && rowSort.by === "value" && key(rowSort.col) === key(leaf.agg) &&
      rowSort.vi === v ? rowSort.dir : "";
    return {
      sort: JSON.stringify([leaf.agg, v]), ci: String(ci),
      sort_icon: dir === "asc" ? "▲" : dir === "desc" ? "▼" : "",
      aria_sort: dir === "asc" ? "ascending" : dir === "desc" ? "descending" : ""
    };
  };
  const valueRow = vcEff > 1 || !hasCols;
  const leafCells = (level: number): object[] =>
    walk.cells.filter((c) => c.lvl === level).map((c) => {
      let extra: SortAttrs;
      if (!valueRow && c.leaf) {
        const ci = leaves.indexOf(c.leaf);
        extra = sortAttrs(leaves[ci], 0, ci);
      } else {
        extra = noSort();
      }
      return { cls: c.cls, colspan: c.colspan, rowspan: c.rowspan, path: c.path, label: c.label,
               toggle: c.toggle, expanded: c.expanded, ...extra };
    });
  const grandCell = (): object => {
    const base = { cls: "ah-pg-col-th ah-pg-grand-total", colspan: span(vcEff), rowspan: span(D),
                   path: "", toggle: "", expanded: "", label: grand };
    return { ...base, ...(valueRow ? noSort() : sortAttrs(leaves[leaves.length - 1], 0, leaves.length - 1)) };
  };
  const hrows: object[] = [];
  for (let lv = 1; lv <= D; lv++) {
    hrows.push({ cls: "ah-pg-col-header-row",
                 cells: leafCells(lv).concat(lv === 1 && gt ? [grandCell()] : []) });
  }
  if (valueRow) {
    hrows.push({
      cls: "ah-pg-col-header-row ah-pg-value-label-row",
      cells: phys.map((pv, ci) => ({
        cls: "ah-pg-col-th ah-pg-value-label" + leafCls(pv[0]),
        colspan: "", rowspan: "", path: "", toggle: "", expanded: "",
        label: !hasCols && vor ? grand : valueLabel(M, vs[pv[1]]),
        ...sortAttrs(pv[0], pv[1], ci)
      }))
    });
  }
  const entries: RowEntry[] = hasRows ? (rowList as RowEntry[]).concat(gt ? [{ grand: true }] : []) : [{ grand: true }];
  const visRows: [RowEntry, number][] = [];
  entries.forEach((e) => {
    if (vor) { for (let v = 0; v < vc; v++) { visRows.push([e, v]); } } else { visRows.push([e, 0]); }
  });
  const isTotal = (e: RowEntry): boolean => e.grand ? true : e.exp && M.row_subtotals;
  const rows = visRows.map(([e, v]) => {
    const total = isTotal(e);
    const head = v !== 0 ? [] : [e.grand
      ? { rowspan: vor ? span(vc) : "", expanded: "", indent: "0", toggle: "", label: grand }
      : { rowspan: vor ? span(vc) : "",
          expanded: !e.kids ? "" : e.exp ? "true" : "false",
          indent: String(e.depth * 20),
          toggle: !e.kids ? "ah-pg-toggle ah-pg-toggle-leaf"
            : e.exp ? "ah-pg-toggle ah-pg-toggle-open" : "ah-pg-toggle ah-pg-toggle-closed",
          label: keyLabel(M, e.key) }];
    return {
      cls: "ah-pg-row-header" + tot(total), path: e.grand ? "[]" : key(e.path),
      vi: vor ? String(v) : "", head, vlabel: vor ? valueLabel(M, vs[v]) : ""
    };
  });
  const body = visRows.map(([e, ev], ri) => {
    const total = isTotal(e), rp = e.grand ? [] : e.path;
    const blank = !e.grand && !!e.exp && !M.row_subtotals;
    return {
      cls: "ah-pg-body-row" + tot(total),
      cells: phys.map((pv, ci) => {
        const leaf = pv[0], v = vor ? ev : pv[1];
        const val = blank ? null : valueAt(E, vs, rp, leaf.agg, v);
        return {
          id: M.id + "-c" + ri + "-" + ci,
          cls: "ah-pg-cell" + (leaf.kind === "grand" ? " ah-pg-grand-total"
            : leaf.kind === "subtotal" ? " ah-pg-total" : tot(total)),
          v: raw(val), text: formatValue(M, val, vs[v])
        };
      })
    };
  });
  return {
    empty: "",
    corner: M.layout.rows.map((f) => fieldLabel(M, f)).join(" / "),
    cols: phys.map(() => ({})),
    hrows, rows, body
  };
}

function fieldsView(M: Model): object {
  const L = M.labels, used = M.layout.rows.concat(M.layout.columns);
  const zone = (z: string, chips: [string, string][]): object => ({
    zone: z, label: L[z], empty: L.drop,
    chips: chips.map((c, i) => ({ id: M.id + "-chip-" + z + "-" + i, zone: z, index: String(i),
                                   field: c[0], label: c[1] }))
  });
  return {
    zones: [
      zone("fields", M.fields.filter((f) => used.indexOf(f.name) < 0).map((f): [string, string] => [f.name, f.label])),
      zone("rows", M.layout.rows.map((f): [string, string] => [f, fieldLabel(M, f)])),
      zone("columns", M.layout.columns.map((f): [string, string] => [f, fieldLabel(M, f)])),
      zone("values", M.layout.values.map((v): [string, string] => [v.field === null ? "" : v.field, valueLabel(M, v)]))
    ]
  };
}

// The template data of both templates for model M and rows, and the
// view with "all" expansions resolved.
function views(M: Model, rows: readonly Key[][]): { grid: object; fields: object; view: View } {
  const vs = effValues(M);
  const E = aggregate(M, rows, vs);
  const rexp = expandedSet(M.view.expanded_rows, E.rkids);
  const cexp = expandedSet(M.view.expanded_cols, E.ckids);
  const rs = M.view.row_sort;
  const rsort: SortFun = rs && rs.by === "value"
    ? (parent, ks) => ks.sort(valueCmp(E, vs, parent, rs))
    : (_parent, ks) => ks.sort(keyCmp(rs ? rs.dir : "asc"));
  const csort: SortFun = (_parent, ks) => ks.sort(keyCmp(M.view.col_sort));
  const rowList = flatten(E.rkids, rexp, rsort, [], 0, []);
  const colList = flatten(E.ckids, cexp, csort, [], 0, []);
  const view: View = {
    ...M.view,
    expanded_rows: Array.from(rexp).map((k) => toPath(JSON.parse(k))),
    expanded_cols: Array.from(cexp).map((k) => toPath(JSON.parse(k)))
  };
  const grid = rows.length ? gridView(M, vs, E, rowList, colList, cexp, csort)
    : { empty: M.labels.empty };
  return { grid, fields: fieldsView(M), view };
}

// ------------------------------------------------------------------
// DOM helpers
// ------------------------------------------------------------------

function kids<E extends Element = HTMLElement>(el: Element | null, sel: string): E[] {
  if (!el) { return []; }
  return Array.prototype.filter.call(el.children, (c: Element) => c.matches(sel)) as E[];
}
function qa<E extends Element = HTMLElement>(el: ParentNode | null, sel: string): E[] {
  return el ? Array.from(el.querySelectorAll<E>(sel)) : [];
}

// The nearest element matching sel from the event target, inside root.
function hit<E extends Element = HTMLElement>(e: Event, sel: string, root: Element): E | null {
  const t = e.target;
  const m = t instanceof Element ? t.closest<E>(sel) : null;
  return m && root.contains(m) ? m : null;
}

function rect(a: RC, b: RC): RC[] {
  const out: RC[] = [];
  for (let r = Math.min(a[0], b[0]); r <= Math.max(a[0], b[0]); r++) {
    for (let c = Math.min(a[1], b[1]); c <= Math.max(a[1], b[1]); c++) { out.push([r, c]); }
  }
  return out;
}

function dropIndex(zone: Element, x: number, y: number): number {
  const chips = kids(zone, ".ah-pg-chip");
  for (let i = 0; i < chips.length; i++) {
    const r = chips[i].getBoundingClientRect();
    if (y < r.top) { return i; }
    if (y <= r.bottom && x < r.left + r.width / 2) { return i; }
  }
  return chips.length;
}

function download(blob: Blob, filename: string): void {
  const url = URL.createObjectURL(blob), a = document.createElement("a");
  a.href = url; a.download = filename;
  document.body.appendChild(a); a.click(); document.body.removeChild(a);
  setTimeout(() => { URL.revokeObjectURL(url); }, 0);
}

// The [path, value index] of a column header's data-sort.
function sortOf(th: Element): [Path, number] {
  const v = JSON.parse(th.getAttribute("data-sort") || "null") as unknown;
  return Array.isArray(v) ? [toPath(v[0]), Number(v[1]) || 0] : [[], 0];
}

function pathOf(el: Element): Path { return toPath(JSON.parse(el.getAttribute("data-path") || "[]")); }

// ------------------------------------------------------------------
// Controller
// ------------------------------------------------------------------

class PivotgridController extends AH.Controller {
  #remote = false;
  #config: Config = parseConfig(null);
  #layout: Layout = { rows: [], columns: [], values: [] };
  #view: View = {};
  #rows: Key[][] = [];
  #sel: RC[] = [];
  #focus: RC | null = null;
  #anchor: RC | null = null;
  #widths: Record<string, number> = {};
  #menu: OpenMenu | null = null;
  #float: FloatHandle | null = null;
  #drag: { zone: string; index: number } | null = null;
  #resizeOff: AbortController | null = null;
  #ro: ResizeObserver | null = null;

  override setup(): void {
    const el = this.element;
    const island = kids(el, "script.ah-pg-data")[0];
    this.#remote = el.hasAttribute("data-ah-remote");
    this.#config = parseConfig(parseJson(el, "data-config"));
    this.#layout = parseLayout(parseJson(el, "data-ah-value"));
    this.#view = parseView(parseJson(el, "data-view")) || {};
    this.#rows = [];
    this.#sel = []; this.#focus = null; this.#anchor = null;
    this.#widths = {}; this.#menu = null; this.#float = null; this.#drag = null;
    if (island) {
      try {
        const data: unknown = JSON.parse(island.textContent || "");
        const rows = isRecord(data) && Array.isArray(data.rows) ? data.rows : [];
        this.#rows = rows.map((t: unknown) => (Array.isArray(t) ? t : []).map(cell));
      } catch (err) {
        this.#rows = [];
      }
    }
    const c = this.#content;

    if (c) {
      // clicks, the innermost target first (the toggle keeps its click
      // from the header it sits in)
      this.listen(c, "click", (e) => {
        let m: HTMLElement | null;
        if ((m = hit(e, ".ah-pg-toggle", c))) {
          e.stopPropagation();
          if (m.classList.contains("ah-pg-toggle-leaf")) { return; }
          const holder = m.closest("[data-path]");
          if (holder) { this.#toggle(m.closest(".ah-pg-row-headers") ? "row" : "col", pathOf(holder)); }
        } else if ((m = hit(e, "th[data-sort]", c))) {
          if (hit(e, ".ah-pg-resize-handle", m)) { return; }
          const sort = sortOf(m);
          this.#cycleSort(sort[0], sort[1]);
        } else if ((m = hit<HTMLTableCellElement>(e, "td.ah-pg-cell", c))) {
          const td = m as HTMLTableCellElement;
          const rc: RC = [(td.parentElement as HTMLTableRowElement).sectionRowIndex, td.cellIndex];
          this.#select(rc, e.shiftKey ? "range" : (e.ctrlKey || e.metaKey) ? "toggle" : null);
          this.#fireCell(td);
        }
      });
      this.delegate("mousedown", ".ah-pg-resize-handle", (e, h) => {
        const th = h.closest("th");
        if (th) { this.#startResize(e, th); }
      }, c);
      this.delegate("contextmenu", "th.ah-pg-col-th, .ah-pg-row-header, td.ah-pg-cell", (e, t) => {
        e.preventDefault();
        const ctx = t.matches("td.ah-pg-cell") ? "cell" : t.matches("th") ? "col" : "row";
        if (ctx === "cell") {
          const td = t as HTMLTableCellElement;
          this.#select([(td.parentElement as HTMLTableRowElement).sectionRowIndex, td.cellIndex], null);
        }
        this.#openMenu(ctx, t, { x: e.clientX, y: e.clientY });
      }, c);
      this.listen(c, "keydown", (e) => {
        if (e.target === c) { this.#onKey(e); }
      });
      this.listen(c, "focus", () => {
        if (!this.#focus && this.#bodyRows().length) { this.#focus = [0, 0]; this.#paint(); }
      });
      this.delegate("wheel", ".ah-pg-row-headers, .ah-pg-col-headers", (e) => {
        const body = c.querySelector(".ah-pg-body");
        if (!body) { return; }
        const t0 = body.scrollTop, l0 = body.scrollLeft;
        body.scrollTop += e.deltaY;
        body.scrollLeft += e.deltaX;
        if (body.scrollTop !== t0 || body.scrollLeft !== l0) { e.preventDefault(); }
      }, c);
      this.listen(c, "scroll", (e) => {
        if (e.target instanceof Element && e.target.classList.contains("ah-pg-body")) { this.#syncScroll(); }
      }, { capture: true });
    }

    // context menu
    const m = this.#menuEl;
    if (m) {
      this.delegate("click", ".ah-pg-context-menu-item", (_e, item) => { this.#menuAction(item); }, m);
      this.listen(m, "keydown", (e) => { this.#onMenuKey(e); });
      this.listen(document, "mousedown", (e) => {
        if (this.#menu && !m.contains(e.target as Node | null)) { this.#closeMenu(false); }
      });
    }

    // field list
    const f = this.#fieldsEl;
    if (f) {
      this.delegate("click", ".ah-pg-chip", (_e, chip) => {
        this.#openMenu(chip.getAttribute("data-zone") || "", chip, null);
      }, f);
      this.delegate("keydown", ".ah-pg-chip", (e, chip) => {
        const zone = chip.getAttribute("data-zone") || "", idx = +(chip.getAttribute("data-index") || "");
        if (e.key === "Enter" || e.key === " " || e.key === "ContextMenu" || (e.key === "F10" && e.shiftKey)) {
          e.preventDefault();
          this.#openMenu(zone, chip, null);
        } else if ((e.key === "Delete" || e.key === "Backspace") && zone !== "fields") {
          e.preventDefault();
          this.#moveField(zone, idx, "fields", -1);
        } else if (e.key === "ArrowRight" || e.key === "ArrowLeft") {
          e.preventDefault();
          const chips = qa(f, ".ah-pg-chip"), i = chips.indexOf(chip);
          const next = chips[i + (e.key === "ArrowRight" ? 1 : -1)];
          if (next) { next.focus(); }
        }
      }, f);
      this.delegate("dragstart", ".ah-pg-chip", (e, chip) => {
        this.#drag = { zone: chip.getAttribute("data-zone") || "", index: +(chip.getAttribute("data-index") || "") };
        const dt = e.dataTransfer;
        if (dt) { dt.effectAllowed = "move"; dt.setData("text/plain", chip.getAttribute("data-field") || ""); }
        chip.classList.add("ah-pg-chip-dragging");
      }, f);
      this.delegate("dragend", ".ah-pg-chip", () => {
        this.#drag = null;
        qa(f, ".ah-pg-chip-dragging").forEach((x) => { x.classList.remove("ah-pg-chip-dragging"); });
        qa(f, ".ah-pg-zone-over").forEach((x) => { x.classList.remove("ah-pg-zone-over"); });
      }, f);
      this.delegate("dragover", ".ah-pg-zone", (e, zone) => {
        if (!this.#drag) { return; }
        e.preventDefault();
        qa(f, ".ah-pg-zone-over").forEach((x) => { if (x !== zone) { x.classList.remove("ah-pg-zone-over"); } });
        zone.classList.add("ah-pg-zone-over");
      }, f);
      this.delegate("dragleave", ".ah-pg-zone", (e, zone) => {
        const rel = e.relatedTarget as Node | null;
        if (!(rel && zone.contains(rel))) { zone.classList.remove("ah-pg-zone-over"); }
      }, f);
      this.delegate("drop", ".ah-pg-zone", (e, zone) => {
        if (!this.#drag) { return; }
        e.preventDefault();
        const d = this.#drag;
        this.#drag = null;
        zone.classList.remove("ah-pg-zone-over");
        const into = zone.getAttribute("data-zone") || "";
        if (into === "fields" && d.zone === "fields") { return; }
        this.#moveField(d.zone, d.index, into, dropIndex(zone, e.clientX, e.clientY));
      }, f);
    }

    if (window.ResizeObserver && c) {
      let lastW = -1;
      this.#ro = new ResizeObserver((entries) => {
        const w = Math.round(entries[0].contentRect.width);
        if (w !== lastW) { lastW = w; this.#syncLayout(); }
      });
      this.#ro.observe(c);
    }
    this.#syncLayout();
  }

  override teardown(): void {
    if (this.#resizeOff) { this.#resizeOff.abort(); this.#resizeOff = null; }
    if (this.#ro) { this.#ro.disconnect(); }
    if (this.#float) { this.#float.stop(); this.#float = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  expandAll(): void { this.#expandAll(true); }
  collapseAll(): void { this.#expandAll(false); }
  expandRow(p: unknown): void { this.#toggle("row", toPath(p), true); }
  collapseRow(p: unknown): void { this.#toggle("row", toPath(p), false); }
  expandColumn(p: unknown): void { this.#toggle("col", toPath(p), true); }
  collapseColumn(p: unknown): void { this.#toggle("col", toPath(p), false); }
  sortRows(dir?: unknown): void { this.#setView({ row_sort: dir ? { by: "key", dir: String(dir) } : null }); }
  sortByValue(col?: unknown, vi?: unknown, dir?: unknown): void {
    this.#sortByValue(toPath(col), Number(vi) || 0, dir ? String(dir) : "asc");
  }
  sortColumns(dir?: unknown): void { this.#setView({ col_sort: dir === "desc" ? "desc" : "asc" }); }
  setLayout(layout: unknown): void { this.#setLayout(toLayoutInput(layout), false); }
  getLayout(): Layout { return this.#layout; }
  getView(): View { return this.#view; }
  setData(rows: unknown): void {
    const names = this.#config.fields.map((f) => f.name);
    this.#rows = (Array.isArray(rows) ? rows : []).map((o: unknown) => {
      const r = isRecord(o) ? o : {};
      return names.map((n) => cell(r[n]));
    });
    if (!this.#remote) { this.#render(); }
  }
  getSelection(): SelectionEvent { return this.#selection(); }
  clearSelection(): void { this.#sel = []; this.#focus = null; this.#paint(); }
  exportXlsx(opts?: unknown): Promise<Blob> { return this.#exportXlsx(toOptions(opts)); }
  exportCsv(opts?: unknown): string { return this.#exportCsv(toOptions(opts)); }
  exportData(): ExportTable { return this.#exportData(); }
  refresh(): void { this.#update(false); }
  // the HTML of both templates for the current state (local mode)
  viewHtml(): { grid: string; fields: string } {
    const out = views(this.#model(), this.#rows);
    return { grid: AH.tpl.pivotgrid_grid(out.grid), fields: AH.tpl.pivotgrid_fields(out.fields) };
  }
  viewLoaded(): void {
    const el = this.element;
    this.#view = parseView(parseJson(el, "data-view")) || this.#view;
    el.classList.remove("ah-pg-loading");
    el.removeAttribute("aria-busy");
    const active = document.activeElement;
    this.#afterRender(undefined, undefined, active && el.contains(active) ? active.id : null);
  }

  // ---- parts ----------------------------------------------------------

  get #content(): HTMLElement | null { return document.getElementById(this.element.id + "-content"); }
  get #menuEl(): HTMLElement | null { return document.getElementById(this.element.id + "-menu"); }
  get #fieldsEl(): HTMLElement | null { return document.getElementById(this.element.id + "-fields"); }
  #menuItemsShown(): HTMLElement[] { return kids(this.#menuEl, "*").filter((i) => !i.hidden); }

  #model(): Model {
    const c = this.#config;
    return {
      id: this.element.id, fields: c.fields,
      fieldNames: c.fields.map((f) => f.name),
      layout: this.#layout, view: this.#view,
      row_subtotals: c.row_subtotals, col_subtotals: c.col_subtotals,
      grand_totals: c.grand_totals, values_on_rows: c.values_on_rows,
      format: c.format, labels: c.labels
    };
  }

  // ---- layout of the four quadrants ------------------------------

  // Give the columns of the header and body tables the same widths (the
  // wider of the two natural ones, or the width the user dragged), the
  // row header and body rows the same heights, and leave room for the
  // body's scroll bars in the header panes.
  #syncLayout(): void {
    const c = this.#content;
    if (!c) { return; }
    const colT = c.querySelector<HTMLTableElement>(".ah-pg-col-table");
    const bodyT = c.querySelector<HTMLTableElement>(".ah-pg-body-table");
    const body = c.querySelector<HTMLElement>(".ah-pg-body");
    if (!colT || !bodyT || !body) { return; }
    const hcols = colT.querySelectorAll<HTMLElement>("col"), bcols = bodyT.querySelectorAll<HTMLElement>("col");
    [colT, bodyT].forEach((t) => {
      t.style.width = ""; t.style.tableLayout = ""; t.style.minWidth = "0";
    });
    for (let i = 0; i < hcols.length; i++) { hcols[i].style.width = ""; bcols[i].style.width = ""; }
    const n = hcols.length, keys: string[] = new Array<string>(n).fill("");
    let w: number[] = new Array<number>(n).fill(0);
    qa(colT, "th[data-ci]").forEach((th) => {
      const ci = +(th.getAttribute("data-ci") || "");
      w[ci] = Math.max(w[ci], th.getBoundingClientRect().width);
      keys[ci] = th.getAttribute("data-sort") || "";
    });
    const first = bodyT.rows[0];
    if (first) {
      for (let j = 0; j < first.cells.length && j < n; j++) {
        w[j] = Math.max(w[j], first.cells[j].getBoundingClientRect().width);
      }
    }
    for (let k = 0; k < n; k++) {
      if (this.#widths[keys[k]]) { w[k] = this.#widths[keys[k]]; }
      w[k] = Math.ceil(w[k]);
    }
    let sum = w.reduce((a, b) => a + b, 0);
    const avail = body.clientWidth;
    if (sum < avail && n) {
      const extra = Math.floor((avail - sum) / n);
      w = w.map((x, i2) => x + extra + (i2 === n - 1 ? (avail - sum) - extra * n : 0));
      sum = avail;
    }
    for (let m = 0; m < n; m++) { hcols[m].style.width = w[m] + "px"; bcols[m].style.width = w[m] + "px"; }
    [colT, bodyT].forEach((t) => {
      t.style.tableLayout = "fixed"; t.style.width = sum + "px"; t.style.minWidth = "";
    });
    const rtrs = c.querySelectorAll<HTMLElement>(".ah-pg-row-table > tbody > tr");
    const btrs: ArrayLike<HTMLElement> = bodyT.tBodies[0] ? bodyT.tBodies[0].rows : [];
    for (let r = 0; r < rtrs.length && r < btrs.length; r++) {
      rtrs[r].style.height = ""; btrs[r].style.height = "";
    }
    for (let q = 0; q < rtrs.length && q < btrs.length; q++) {
      const h1 = rtrs[q].getBoundingClientRect().height, h2 = btrs[q].getBoundingClientRect().height;
      if (Math.abs(h1 - h2) > 0.5) {
        const h = Math.max(h1, h2) + "px";
        rtrs[q].style.height = h; btrs[q].style.height = h;
      }
    }
    const rowT = c.querySelector<HTMLElement>(".ah-pg-row-table");
    if (rowT) { rowT.style.marginBottom = (body.offsetHeight - body.clientHeight) + "px"; }
    colT.style.marginRight = (body.offsetWidth - body.clientWidth) + "px";
    this.#syncScroll();
  }

  #syncScroll(): void {
    const c = this.#content;
    const body = c && c.querySelector(".ah-pg-body");
    if (!c || !body) { return; }
    const ch = c.querySelector(".ah-pg-col-headers"), rh = c.querySelector(".ah-pg-row-headers");
    if (ch) { ch.scrollLeft = body.scrollLeft; }
    if (rh) { rh.scrollTop = body.scrollTop; }
  }

  // ---- rendering and view changes --------------------------------

  #writeState(): void {
    const el = this.element;
    const layout = JSON.stringify(this.#layout);
    el.setAttribute("data-ah-value", layout);
    el.setAttribute("data-view", JSON.stringify(this.#view));
    kids<HTMLInputElement>(el, "input[type=hidden]").forEach((i) => { i.value = layout; });
  }

  #render(): void {
    const c = this.#content;
    if (!c) { return; }
    const body = c.querySelector(".ah-pg-body");
    const sl = body ? body.scrollLeft : 0, stp = body ? body.scrollTop : 0;
    const active = document.activeElement;
    const focused = active && this.element.contains(active) ? active.id : null;
    const out = views(this.#model(), this.#rows);
    this.#view = out.view;
    c.innerHTML = AH.tpl.pivotgrid_grid(out.grid);
    const f = this.#fieldsEl;
    if (f) { f.innerHTML = AH.tpl.pivotgrid_fields(out.fields); }
    this.#afterRender(sl, stp, focused);
  }

  #afterRender(sl: number | undefined, stp: number | undefined, focused: string | null): void {
    this.#sel = []; this.#focus = null; this.#anchor = null;
    const c = this.#content;
    if (c) { c.removeAttribute("aria-activedescendant"); }
    this.#syncLayout();
    const body = c && c.querySelector(".ah-pg-body");
    if (body && sl !== undefined && stp !== undefined) { body.scrollLeft = sl; body.scrollTop = stp; this.#syncScroll(); }
    if (focused) {
      const again = document.getElementById(focused);
      if (again) { again.focus(); }
    }
  }

  // A view change: re-render here (local) or ask the server (remote);
  // 'ah:view' fires in both cases, `change' too when the layout changed.
  #update(layoutChanged: boolean): void {
    const el = this.element;
    if (this.#remote) {
      this.#writeState();
      el.classList.add("ah-pg-loading");
      el.setAttribute("aria-busy", "true");
    } else {
      this.#render();
      this.#writeState();
    }
    if (layoutChanged) { this.fire("change"); }
    this.fire<ViewEvent>("ah:view", { layout: this.#layout, view: this.#view });
  }

  #setView(patch: View): void {
    this.#view = { ...this.#view, ...patch };
    this.#update(false);
  }

  #toggle(axis: "row" | "col", p: Path, open?: boolean): void {
    const cur = axis === "row" ? this.#view.expanded_rows : this.#view.expanded_cols;
    let list = cur === "all" ? this.#allPaths(axis) : (cur || []).slice();
    const k = key(p);
    const has = list.some((q) => key(q) === k);
    const want = open === undefined ? !has : open;
    if (want === has) { return; }
    list = want ? list.concat([p]) : list.filter((q) => key(q) !== k);
    this.#setView(axis === "row" ? { expanded_rows: list } : { expanded_cols: list });
  }

  // every expandable path (local: from the data; remote: the view asks
  // the server for "all")
  #allPaths(axis: "row" | "col"): Path[] {
    if (this.#remote) {
      const sel = axis === "row" ? ".ah-pg-row-headers td[aria-expanded=true]"
        : ".ah-pg-col-headers th[aria-expanded=true]";
      return qa(this.#content, sel).map((c) => {
        const holder = c.closest("[data-path]");
        return holder ? pathOf(holder) : [];
      });
    }
    const M = this.#model();
    const E = aggregate(M, this.#rows, effValues(M));
    return Array.from(expandedSet("all", axis === "row" ? E.rkids : E.ckids)).map((k) => toPath(JSON.parse(k)));
  }

  #expandAll(open: boolean): void {
    this.#setView(open ? { expanded_rows: this.#remote ? "all" : this.#allPaths("row"),
                           expanded_cols: this.#remote ? "all" : this.#allPaths("col") }
                       : { expanded_rows: [], expanded_cols: [] });
  }

  #sortByValue(col: Path, vi: number, dir: string | null): void {
    this.#setView({ row_sort: dir ? { by: "value", dir, col, vi } : null });
  }

  // clicking a column's bottom header: ascending, descending, unsorted
  #cycleSort(col: Path, vi: number): void {
    const cur = this.#view.row_sort;
    const same = !!cur && cur.by === "value" && key(cur.col) === key(col) && cur.vi === vi;
    this.#sortByValue(col, vi, !same || !cur ? "asc" : cur.dir === "asc" ? "desc" : null);
  }

  // ---- layout (the field list) -----------------------------------

  #defaultAgg(field: string | null | undefined): string {
    const f = this.#config.fields.filter((x) => x.name === field).pop();
    return f ? f.agg : "sum";
  }

  #setLayout(layout: LayoutInput, fire?: boolean): void {
    const old = this.#layout;
    const norm: Layout = {
      rows: (layout.rows || []).slice(),
      columns: (layout.columns || []).slice(),
      values: (layout.values || []).map((v0) => {
        const v = typeof v0 === "string" ? { field: v0 } : v0;
        const field = v.field === undefined ? null : v.field;
        return { label: v.label === undefined ? null : v.label,
                 agg: v.agg || (field === null ? "count" : this.#defaultAgg(field)),
                 field };
      })
    };
    const view = this.#view;
    const patch: View = {};
    if (key(norm.rows) !== key(old.rows)) {
      patch.expanded_rows = [];
      if (view.row_sort && view.row_sort.by === "value") { patch.row_sort = null; }
    }
    if (key(norm.columns) !== key(old.columns)) {
      patch.expanded_cols = [];
      if (view.row_sort && view.row_sort.by === "value") { patch.row_sort = null; }
    }
    if (view.row_sort && view.row_sort.by === "value" &&
        (view.row_sort.vi || 0) >= Math.max(1, norm.values.length)) {
      patch.row_sort = null;
    }
    this.#layout = norm;
    this.#view = { ...view, ...patch };
    if (fire === false) {
      if (!this.#remote) { this.#render(); }
      this.#writeState();
    } else {
      this.#update(true);
    }
  }

  // Move the chip `index' of zone `from' to position `to' of zone
  // `into' (to = -1: at the end).
  #moveField(from: string, index: number, into: string, to: number): void {
    const L = { rows: this.#layout.rows.slice(), columns: this.#layout.columns.slice(),
                values: this.#layout.values.slice() };
    let field: string | null | undefined, spec: ValueSpec | null = null;
    if (from === "fields") {
      const chip = document.getElementById(this.element.id + "-chip-fields-" + index);
      field = chip ? chip.getAttribute("data-field") : undefined;
    } else if (from === "values") {
      spec = L.values.splice(index, 1)[0];
      field = spec ? spec.field : undefined;
    } else if (from === "rows" || from === "columns") {
      field = L[from].splice(index, 1)[0];
    }
    if (into === "fields") { this.#setLayout(L); return; }
    const ins = <T>(list: T[], item: T): void => {
      let at = to < 0 || to > list.length ? list.length : to;
      if (from === into && index < at && to >= 0) { at--; }
      list.splice(at, 0, item);
    };
    if (into === "values") {
      if (field === null && !spec) { return; }
      ins<ValueSpec>(L.values, spec && from === "values" ? spec
          : { field: field === undefined ? null : field, agg: this.#defaultAgg(field), label: null });
    } else if (into === "rows" || into === "columns") {
      if (field === null || field === undefined) { return; }
      const name = field;
      (["rows", "columns"] as const).forEach((z) => {
        const i = L[z].indexOf(name);
        if (i >= 0) { L[z].splice(i, 1); }
      });
      ins(L[into], name);
    } else {
      return;
    }
    this.#setLayout(L);
  }

  // ---- selection, focus, cell events -----------------------------

  #bodyRows(): ArrayLike<HTMLTableRowElement> {
    const c = this.#content;
    const t = c && c.querySelector<HTMLTableElement>(".ah-pg-body-table");
    return t && t.tBodies[0] ? t.tBodies[0].rows : [];
  }

  #cellAt(r: number, c: number): HTMLTableCellElement | null {
    const rows = this.#bodyRows();
    return rows[r] ? rows[r].cells[c] || null : null;
  }

  // The members, measure and value of a body cell, read from the DOM.
  #cellInfo(td: HTMLTableCellElement): CellInfo {
    const tr = td.parentElement as HTMLTableRowElement, r = tr.sectionRowIndex, c = td.cellIndex;
    const content = this.#content as HTMLElement;   // the cell is in it
    const rh = content.querySelectorAll(".ah-pg-row-table > tbody > tr")[r];
    const th = content.querySelector('.ah-pg-col-table th[data-ci="' + c + '"]');
    const row = rh ? pathOf(rh) : [];
    const sort: [Path, number] = th ? sortOf(th) : [[], 0];
    const vi = rh && rh.hasAttribute("data-vi") ? +(rh.getAttribute("data-vi") || "") : sort[1];
    const vs: { field: string | null; agg: string }[] = this.#layout.values.length
      ? this.#layout.values : [{ field: null, agg: "count" }];
    const filter: Record<string, Key> = {};
    row.forEach((k, i) => { filter[this.#layout.rows[i]] = k; });
    sort[0].forEach((k, i) => { filter[this.#layout.columns[i]] = k; });
    const v = td.getAttribute("data-v");
    return { row, col: sort[0], vi, r, c, filter,
             field: vs[vi] ? vs[vi].field : null, agg: vs[vi] ? vs[vi].agg : "count",
             value: v === null ? null : +v, text: td.textContent || "" };
  }

  #paint(): void {
    const c = this.#content;
    if (!c) { return; }
    qa(c, ".ah-pg-cell-selected").forEach((td) => {
      td.classList.remove("ah-pg-cell-selected");
      td.removeAttribute("aria-selected");
    });
    qa(c, ".ah-pg-cell-focused").forEach((td) => {
      td.classList.remove("ah-pg-cell-focused");
      td.removeAttribute("aria-label");
    });
    this.#sel.forEach((rc) => {
      const td = this.#cellAt(rc[0], rc[1]);
      if (td) { td.classList.add("ah-pg-cell-selected"); td.setAttribute("aria-selected", "true"); }
    });
    const focusTd = this.#focus ? this.#cellAt(this.#focus[0], this.#focus[1]) : null;
    if (focusTd) {
      const info = this.#cellInfo(focusTd), M = this.#model();
      const rl = info.row.length ? info.row.map((k) => keyLabel(M, k)).join(" / ") : M.labels.grand_total;
      const cl = info.col.length ? info.col.map((k) => keyLabel(M, k)).join(" / ") : M.labels.grand_total;
      const vs = effValues(M);
      focusTd.classList.add("ah-pg-cell-focused");
      focusTd.setAttribute("aria-label", rl + ", " + cl + ", " +
                           valueLabel(M, vs[Math.min(info.vi, vs.length - 1)]) + ": " + (info.text || "-"));
      c.setAttribute("aria-activedescendant", focusTd.id);
      this.#scrollIntoView(focusTd);
    } else if (!this.#focus) {
      c.removeAttribute("aria-activedescendant");
    }
  }

  #scrollIntoView(td: HTMLTableCellElement): void {
    const c = this.#content;
    const body = c && c.querySelector<HTMLElement>(".ah-pg-body");
    if (!body) { return; }
    const tr = td.parentElement as HTMLTableRowElement;
    const top = tr.offsetTop, h = tr.offsetHeight;
    if (top < body.scrollTop) { body.scrollTop = top; }
    if (top + h > body.scrollTop + body.clientHeight) { body.scrollTop = top + h - body.clientHeight; }
    const left = td.offsetLeft, w = td.offsetWidth;
    if (left < body.scrollLeft) { body.scrollLeft = left; }
    if (left + w > body.scrollLeft + body.clientWidth) { body.scrollLeft = left + w - body.clientWidth; }
  }

  #select(rc: RC, mode: "range" | "toggle" | null): void {
    if (mode === "range" && this.#anchor) {
      this.#sel = rect(this.#anchor, rc);
    } else if (mode === "toggle") {
      const i = this.#sel.findIndex((x) => x[0] === rc[0] && x[1] === rc[1]);
      if (i >= 0) { this.#sel.splice(i, 1); } else { this.#sel.push(rc); }
      this.#anchor = rc;
    } else {
      this.#sel = [rc];
      this.#anchor = rc;
    }
    this.#focus = rc;
    this.#paint();
    this.fire<SelectionEvent>("ah:selection", this.#selection());
  }

  #selection(): CellRef[] {
    const out: CellRef[] = [];
    this.#sel.forEach((rc) => {
      const td = this.#cellAt(rc[0], rc[1]);
      if (!td) { return; }
      const i = this.#cellInfo(td);
      out.push({ row: i.row, col: i.col, vi: i.vi, value: i.value });
    });
    return out;
  }

  // 'ah:cell-click': the cell's JSON goes into data-cell for the action's
  // Event.data while the event runs.
  #fireCell(td: HTMLTableCellElement): void {
    const el = this.element;
    const i = this.#cellInfo(td);
    const detail: CellClickEvent = { row: i.row, col: i.col, filter: i.filter, field: i.field, agg: i.agg,
                                     value: i.value, text: i.text };
    el.setAttribute("data-cell", JSON.stringify(detail));
    try {
      this.fire<CellClickEvent>("ah:cell-click", detail);
    } finally {
      el.removeAttribute("data-cell");
    }
  }

  // ---- context menu ----------------------------------------------

  #openMenu(ctx: string, target: HTMLElement, at: { x: number; y: number } | null): void {
    const m = this.#menuEl;
    if (!m) { return; }
    this.#menu = { ctx, target, invoker: document.activeElement };
    kids(m, "*").forEach((item) => {
      const ctxs = (item.getAttribute("data-ctx") || "").split(" ");
      let show = ctxs.indexOf(ctx) >= 0;
      const act = item.getAttribute("data-action") || "";
      if (show && ctx === "values" && act === "agg") {
        const spec = this.#layout.values[+(target.getAttribute("data-index") || "")];
        item.setAttribute("aria-checked", spec && spec.agg === item.getAttribute("data-agg") ? "true" : "false");
        if (spec && spec.field === null && item.getAttribute("data-agg") !== "count") { show = false; }
      }
      if (show && (act === "move-left" || act === "move-right")) {
        const idx = +(target.getAttribute("data-index") || "");
        const len = kids(target.parentElement, ".ah-pg-chip").length;
        show = act === "move-left" ? idx > 0 : idx < len - 1;
      }
      if (show && ctx === "col" && /^sort-value/.test(act)) { show = target.hasAttribute("data-sort"); }
      item.hidden = !show;
    });
    m.classList.add("ah-pg-context-menu-open");
    if (at) {
      const w = m.offsetWidth, h = m.offsetHeight;
      const left = at.x + w > window.innerWidth ? Math.max(0, at.x - w) : at.x;
      const top = at.y + h > window.innerHeight ? Math.max(0, at.y - h) : at.y;
      Object.assign(m.style, { position: "fixed", left: left + "px", top: top + "px" });
    } else {
      this.#float = AH.float(m, target, { placement: "bottom", align: "start", offset: 2 });
    }
    const first = this.#menuItemsShown()[0];
    if (first) { first.focus(); }
  }

  #closeMenu(refocus: boolean): void {
    if (!this.#menu) { return; }
    if (this.#float) { this.#float.stop(); this.#float = null; }
    const m = this.#menuEl;
    if (m) { m.classList.remove("ah-pg-context-menu-open"); }
    const inv = this.#menu.invoker;
    this.#menu = null;
    if (refocus && inv instanceof HTMLElement && document.contains(inv)) { inv.focus(); }
  }

  #menuAction(item: HTMLElement): void {
    const m = this.#menu;
    if (!m) { return; }
    const act = item.getAttribute("data-action") || "", t = m.target;
    this.#closeMenu(true);
    const info = m.ctx === "cell" ? this.#cellInfo(t as HTMLTableCellElement) : null;
    const sort: [Path, number] | null = m.ctx === "col" && t.hasAttribute("data-sort") ? sortOf(t)
      : info ? [info.col, info.vi] : null;
    switch (act) {
      case "sort-asc": case "sort-desc":
        this.#setView({ row_sort: { by: "key", dir: act === "sort-asc" ? "asc" : "desc" } });
        return;
      case "sort-value-asc": case "sort-value-desc":
        if (sort) { this.#sortByValue(sort[0], sort[1], act === "sort-value-asc" ? "asc" : "desc"); }
        return;
      case "sort-cols-asc": this.#setView({ col_sort: "asc" }); return;
      case "sort-cols-desc": this.#setView({ col_sort: "desc" }); return;
      case "sort-clear": this.#setView({ row_sort: null, col_sort: "asc" }); return;
      case "expand-all": this.#expandAll(true); return;
      case "collapse-all": this.#expandAll(false); return;
      case "export-xlsx": void this.#exportXlsx({}); return;
      case "export-csv": this.#exportCsv({}); return;
      case "agg": {
        const L = parseLayout(JSON.parse(JSON.stringify(this.#layout))), i = +(t.getAttribute("data-index") || "");
        L.values[i].agg = item.getAttribute("data-agg") || "";
        this.#setLayout(L);
        return;
      }
      default: {
        const zone = t.getAttribute("data-zone") || "", idx = +(t.getAttribute("data-index") || "");
        if (act === "remove") { this.#moveField(zone, idx, "fields", -1); return; }
        if (act === "move-left") { this.#moveField(zone, idx, zone, idx - 1); return; }
        if (act === "move-right") { this.#moveField(zone, idx, zone, idx + 2); return; }
        const into = act.replace("move-", "");
        this.#moveField(zone, idx, into, -1);
      }
    }
  }

  // ---- export (what the table shows, in both modes) ---------------

  #exportData(): ExportTable {
    const c = this.#content, M = this.#model();
    const nr = Math.max(1, this.#layout.rows.length);
    const vor = !!this.#config.values_on_rows && effValues(M).length > 1;
    const off = nr + (vor ? 1 : 0);
    const aoa: (string | number)[][] = [], merges: ExportTable["merges"] = [];
    const occupied: Record<string, boolean> = {};
    const htrs = qa(c, ".ah-pg-col-table > thead > tr");
    const H = htrs.length;
    htrs.forEach((tr, r) => {
      aoa[r] = aoa[r] || [];
      let col = 0;
      kids(tr, "th").forEach((th) => {
        while (occupied[r + "," + col]) { col++; }
        const cs = +(th.getAttribute("colspan") || 1), rs = +(th.getAttribute("rowspan") || 1);
        for (let i = 0; i < rs; i++) {
          for (let j = 0; j < cs; j++) { occupied[(r + i) + "," + (col + j)] = true; }
        }
        aoa[r][off + col] = qa(th, ".ah-pg-col-label").map((l) => l.textContent || "").join("");
        if (cs > 1 || rs > 1) {
          merges.push({ s: { r, c: off + col }, e: { r: r + rs - 1, c: off + col + cs - 1 } });
        }
        col += cs;
      });
    });
    for (let r0 = 0; r0 < H; r0++) {
      for (let k = 0; k < off; k++) { aoa[r0][k] = ""; }
    }
    if (H) {
      this.#layout.rows.forEach((f, i) => { aoa[H - 1][i] = fieldLabel(M, f); });
      if (vor) { aoa[H - 1][nr] = M.labels.values; }
    }
    const rtrs = qa(c, ".ah-pg-row-table > tbody > tr");
    Array.from(this.#bodyRows()).forEach((btr, i) => {
      const rh = rtrs[i], line: (string | number)[] = [];
      const path = rh ? pathOf(rh) : [];
      for (let j = 0; j < nr; j++) { line.push(""); }
      if (!path.length) { line[0] = M.labels.grand_total; }
      path.forEach((key0, j2) => { line[j2] = keyLabel(M, key0); });
      if (vor) {
        line.push(kids(rh, ".ah-pg-value-label-cell").map((l) => l.textContent || "").join(""));
      }
      Array.from(btr.cells).forEach((td) => {
        const v = td.getAttribute("data-v");
        line.push(v === null || v === "" ? (td.textContent || "") : +v);
      });
      aoa.push(line);
    });
    const width = aoa.reduce((m, l) => Math.max(m, l.length), 0);
    for (let x = 0; x < aoa.length; x++) {
      for (let y = 0; y < width; y++) { if (aoa[x][y] === undefined) { aoa[x][y] = ""; } }
    }
    return { aoa, merges };
  }

  // SheetJS is a chunk of its own (vendor-xlsx), fetched on the first
  // Excel export; its licence is in js/THIRD-PARTY-LICENSES.txt.
  #exportXlsx(opts: ExportOptions): Promise<Blob> {
    const d = this.#exportData();
    return AH.vendor("xlsx").then((XLSX) => {
      const ws = XLSX.utils.aoa_to_sheet(d.aoa);
      if (d.merges.length) { ws["!merges"] = d.merges; }
      const wb = XLSX.utils.book_new();
      XLSX.utils.book_append_sheet(wb, ws, opts.sheetName || "Pivot");
      const buf: ArrayBuffer = XLSX.write(wb, { bookType: "xlsx", type: "array" });
      const blob = new Blob([buf], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" });
      if (opts.download !== false) { download(blob, opts.filename || "pivot.xlsx"); }
      return blob;
    });
  }

  #exportCsv(opts: ExportOptions): string {
    const sep = opts.separator || ",";
    const text = this.#exportData().aoa.map((line) => line.map((v) => {
      const t = v === null || v === undefined ? "" : String(v);
      return /[",\r\n]/.test(t) || t.indexOf(sep) >= 0 ? '"' + t.replace(/"/g, '""') + '"' : t;
    }).join(sep)).join("\r\n");
    if (opts.download !== false) {
      download(new Blob(["﻿" + text], { type: "text/csv;charset=utf-8" }), opts.filename || "pivot.csv");
    }
    return text;
  }

  // ---- column resizing -------------------------------------------

  #startResize(e: MouseEvent, th: HTMLElement): void {
    e.preventDefault(); e.stopPropagation();
    const el = this.element;
    const line = kids(el, ".ah-pg-resize-line")[0];
    const root = el.getBoundingClientRect(), cr = th.getBoundingClientRect();
    const x0 = e.clientX, w0 = cr.width;
    let width = w0;
    if (line) {
      Object.assign(line.style, { display: "block", left: (cr.right - root.left) + "px", top: "0",
                                  height: root.height + "px" });
    }
    if (this.#resizeOff) { this.#resizeOff.abort(); }
    const off = this.#resizeOff = new AbortController();
    document.addEventListener("mousemove", (me) => {
      width = Math.max(40, w0 + me.clientX - x0);
      if (line) { line.style.left = (cr.left - root.left + width) + "px"; }
    }, { signal: off.signal });
    document.addEventListener("mouseup", () => {
      off.abort();
      this.#resizeOff = null;
      if (line) { line.style.display = "none"; }
      this.#widths[th.getAttribute("data-sort") || ""] = Math.round(width);
      this.#syncLayout();
    }, { signal: off.signal });
  }

  // ---- keyboard ----------------------------------------------------

  #onKey(e: KeyboardEvent): void {
    const rows = this.#bodyRows();
    if (!rows.length) { return; }
    const maxR = rows.length - 1, maxC = rows[0].cells.length - 1;
    const f = this.#focus || [0, 0], r = f[0], c = f[1], k = e.key;
    const nav: Record<string, RC> = {
      ArrowDown: [r + 1, c], ArrowUp: [r - 1, c], ArrowRight: [r, c + 1], ArrowLeft: [r, c - 1],
      Home: [e.ctrlKey ? 0 : r, 0], End: [e.ctrlKey ? maxR : r, maxC],
      PageDown: [r + 10, c], PageUp: [r - 10, c]
    };
    if (e.altKey && (k === "ArrowRight" || k === "ArrowLeft" || k === "ArrowDown" || k === "ArrowUp")) {
      e.preventDefault();
      const td = this.#focus ? this.#cellAt(r, c) : null;
      if (!td) { return; }
      const info = this.#cellInfo(td);
      if (k === "ArrowRight" || k === "ArrowLeft") {
        if (info.row.length) { this.#toggle("row", info.row, k === "ArrowRight"); }
      } else if (info.col.length) {
        this.#toggle("col", info.col, k === "ArrowDown");
      }
      return;
    }
    if (Object.prototype.hasOwnProperty.call(nav, k)) {
      e.preventDefault();
      const to: RC = [Math.max(0, Math.min(maxR, nav[k][0])), Math.max(0, Math.min(maxC, nav[k][1]))];
      this.#select(to, e.shiftKey ? "range" : null);
      if (e.shiftKey) { this.#focus = to; this.#paint(); }
      return;
    }
    switch (k) {
      case "Tab": {
        if (!this.#focus) { return; }
        const next: RC | null = e.shiftKey ? (c > 0 ? [r, c - 1] : r > 0 ? [r - 1, maxC] : null)
          : (c < maxC ? [r, c + 1] : r < maxR ? [r + 1, 0] : null);
        if (next) { e.preventDefault(); this.#select(next, null); }
        return;
      }
      case "Enter": case " ": {
        e.preventDefault();
        if (!this.#focus) { this.#select([0, 0], null); return; }
        const td = this.#cellAt(r, c);
        if (td) { this.#fireCell(td); }
        return;
      }
      case "+": case "-": {
        if (!this.#focus) { return; }
        e.preventDefault();
        const td = this.#cellAt(r, c);
        if (!td) { return; }
        const i2 = this.#cellInfo(td);
        if (i2.row.length) { this.#toggle("row", i2.row, k === "+"); }
        return;
      }
      case "Escape":
        if (this.#sel.length) {
          e.preventDefault(); this.#sel = []; this.#focus = null; this.#paint();
          this.fire<SelectionEvent>("ah:selection", []);
        }
        return;
      case "ContextMenu": case "F10": {
        if (k === "F10" && !e.shiftKey) { return; }
        e.preventDefault();
        if (!this.#focus) { this.#select([0, 0], null); }
        const fc = this.#focus || [0, 0];
        const td = this.#cellAt(fc[0], fc[1]);
        if (td) { this.#openMenu("cell", td, null); }
        return;
      }
      default:
        if ((e.ctrlKey || e.metaKey) && k === "a") {
          e.preventDefault();
          this.#anchor = [0, 0];
          this.#sel = rect([0, 0], [maxR, maxC]);
          this.#focus = this.#focus || [0, 0];
          this.#paint();
          this.fire<SelectionEvent>("ah:selection", this.#selection());
        }
    }
  }

  #onMenuKey(e: KeyboardEvent): void {
    const items = this.#menuItemsShown();
    const i = items.indexOf(document.activeElement as HTMLElement);
    const n = items.length;
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); if (n) { items[(i + 1) % n].focus(); } break;
      case "ArrowUp": e.preventDefault(); if (n) { items[(i - 1 + n) % n].focus(); } break;
      case "Home": e.preventDefault(); if (n) { items[0].focus(); } break;
      case "End": e.preventDefault(); if (n) { items[n - 1].focus(); } break;
      case "Enter": case " ":
        e.preventDefault();
        if (i >= 0) { this.#menuAction(items[i]); }
        break;
      case "Escape": e.preventDefault(); e.stopPropagation(); this.#closeMenu(true); break;
      case "Tab": this.#closeMenu(false); break;
      default: break;
    }
  }
}

AH.register("pivotgrid", PivotgridController);
