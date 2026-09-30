/* Controller of the data grid (designs/04-components.md).
 *
 * Ported from sigil (data/datagrid). The server renders the header, the
 * rows, the pager and the status bar (aihtml_datagrid); this file keeps
 * the view state and moves that DOM around:
 *
 *   local mode   all rows are in the body; sorting, filtering (filter row,
 *                search box), paging and grouping reorder and hide them
 *                (.ah-dg-row-off), group rows and the pager come from the
 *                shared templates (datagrid_group_row, datagrid_pager),
 *                the status bar aggregates are recomputed
 *   remote mode  (data-ah-remote) the server rendered the first page; every
 *                view change fires ah:query on the grid's hidden
 *                .ah-dg-query element (its data-* carry the view); the
 *                server answers with datagrid_rows/4, which morphs the rows
 *                and the pager in and calls rowsLoaded. Nothing is asked
 *                for on mount (refresh() does that on demand).
 *
 * Pager links (data-ah-href, the `href' option): the pager's buttons are
 * <a href> links for crawlers and new tabs; a plain left click stays in
 * the page (the same re-page or query as a button) and pushes the link's
 * URL to the history (core's url operation: going back reloads it).
 *
 * Also: selection (single / multi / checkbox, data-ah-value + hidden input
 * + change), keyboard navigation over cells (roving tabindex, ARIA grid),
 * column resize, pinning (sticky), hiding, the column menu (template
 * datagrid_column_menu, positioned with AH.float), inline editing
 * (ah:edit), and CSV / Excel / PDF export (xlsx, jspdf and jspdf-autotable
 * through AH.vendor: separate chunks, loaded on the first export).
 *
 * Component events are native CustomEvents (bubbling). Their details are
 * in e.detail (GridEvent: {key, field, value, old, name, expanded},
 * whichever apply) and, while they are dispatched, also data-* attributes
 * of the root, so postbacks bound with on/2 receive them in Event.data:
 *   ah:sort {field, value: "asc" | "desc" | ""}, ah:filter {field, value},
 *   ah:page {value}, ah:group-toggle {value, expanded},
 *   ah:column-resize {field, value}, ah:edit {key, field, value, old},
 *   ah:row-click / ah:row-dblclick {key, field}, ah:command {key, field,
 *   name}, ah:toolbar {name}; ah:query (no detail) on .ah-dg-query.
 */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { join, split } from "./_lib_values.ts";
import "virtual:ah-tpl/datagrid_column_menu";
import "virtual:ah-tpl/datagrid_group_row";
import "virtual:ah-tpl/datagrid_pager";

/** The detail of the grid's events (the fields that apply). */
export interface GridEvent {
  key?: string | null;
  field?: string | null;
  value?: string | number;
  old?: string;
  name?: string | null;
  expanded?: "true" | "false";
}

/** A cell of an export: CSV, Excel and PDF take these. */
export type ExportCell = string | number | boolean | null;

const CHECK = "__checkbox";
const DETAIL = ["key", "field", "value", "old", "name", "expanded"] as const;
const NUM_RE = /^-?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?$/;
const PAGER_BUTTONS = 7;
const CELLS = ".ah-dg-header-cell, .ah-dg-filter-cell, .ah-dg-cell, .ah-dg-statusbar-cell";

/** A column, read from its header cell. */
interface Column {
  field: string;
  head: HTMLElement;
  check: boolean;
  title: string;
  type: string;
  editable: boolean;
  sortable: boolean;
  groupable: boolean;
  pinned: boolean;
  hidden: boolean;
  width: number;
  minWidth: number;
  format: string | null;
  currency: string;
  /** [value, label] of a select column */
  options: [unknown, unknown][];
  aggs: string[];
}

/** A data row of the body. */
interface Rec {
  el: HTMLElement;
  key: string | null;
  cells: Map<string, HTMLElement> | null;
  stamp: number;
}

/** [field, "asc" | "desc"] */
type SortSpec = [string, string];

interface Group {
  id: string;
  level: number;
  title: string;
  count: number;
  recs: Rec[];
  collapsed: boolean;
}

type Item = { row: Rec; group?: undefined } | { group: Group; row?: undefined };

/** The open inline editor. */
interface Editing {
  cell: HTMLElement;
  col: Column;
  old: string;
  ed: HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement;
  content: DocumentFragment;
  off: AbortController;
}

/** The pager template's data (aihtml_datagrid's pager view). */
interface PagerView {
  label: string; info: string;
  prev: number; next: number; last: number;
  at_start: boolean; at_end: boolean;
  first_label: string; prev_label: string; next_label: string; last_label: string;
  pages: { page: number; active: boolean; link?: boolean; href?: string }[];
  size_label: string;
  sizes: { size: number; text: string; selected: boolean }[];
  link?: boolean;
  first_href?: string; prev_href?: string; next_href?: string; last_href?: string;
}

type Labels = Record<string, string>;

// ------------------------------------------------------------------
// Parsing data-* attributes
// ------------------------------------------------------------------

function json(s: string | null): unknown {
  try { return s ? JSON.parse(s) : undefined; } catch (e) { return undefined; }
}

function isRecord(v: unknown): v is Record<string, unknown> {
  return typeof v === "object" && v !== null && !Array.isArray(v);
}

function parseLabels(s: string | null): Labels {
  const v = json(s), out: Labels = {};
  if (isRecord(v)) {
    Object.keys(v).forEach((k) => { if (typeof v[k] === "string") { out[k] = v[k] as string; } });
  }
  return out;
}

function parseSort(s: string | null): SortSpec[] {
  const v = json(s);
  if (!Array.isArray(v)) { return []; }
  return v.filter((sd): sd is unknown[] => Array.isArray(sd) && sd.length >= 2)
    .map((sd): SortSpec => [String(sd[0]), String(sd[1])]);
}

function parseFilters(s: string | null): Record<string, string> {
  const v = json(s), out: Record<string, string> = {};
  if (isRecord(v)) { Object.keys(v).forEach((k) => { out[k] = v[k] == null ? "" : String(v[k]); }); }
  return out;
}

function parseStrings(s: string | null): string[] {
  const v = json(s);
  return Array.isArray(v) ? v.map(String) : [];
}

function parseOptions(s: string | null): [unknown, unknown][] {
  const v = json(s);
  if (!Array.isArray(v)) { return []; }
  return v.filter((o): o is unknown[] => Array.isArray(o)).map((o): [unknown, unknown] => [o[0], o[1]]);
}

// ------------------------------------------------------------------
// Values: raw text (data-v, else the text), comparison, formatting
// ------------------------------------------------------------------

function raw(cell: Element | null): string {
  if (!cell) { return ""; }
  const v = cell.getAttribute("data-v");
  return v === null ? (cell.textContent || "") : v;
}
function text(cell: Element | null): string { return cell ? (cell.textContent || "") : ""; }

function toNumber(s: string): number | null { return NUM_RE.test(s) ? Number(s) : null; }

// The same order as aihtml_datagrid:compare/2.
function compare(a: string, b: string): number {
  const na = toNumber(a), nb = toNumber(b);
  if (na !== null && nb !== null) { return na < nb ? -1 : (na > nb ? 1 : 0); }
  const la = a.toLowerCase(), lb = b.toLowerCase();
  return la < lb ? -1 : (la > lb ? 1 : 0);
}

// with the separators of the page's language (format_value/3 in Erlang)
function thousands(n: number, d: number): string {
  let s = n.toFixed(d), sign = "";
  if (s.charAt(0) === "-") { sign = "-"; s = s.slice(1); }
  const parts = s.split(".");
  return sign + parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, AH.format("group", ",")) +
    (parts[1] ? AH.format("decimal", ".") + parts[1] : "");
}

function pad(n: number, w: number): string {
  let s = String(n);
  while (s.length < w) { s = "0" + s; }
  return s;
}

function dateFormat(v: string, spec: string): string {
  if (!/yyyy|MM|dd|HH|mm|ss/.test(spec)) { return v; }
  const m = /^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?)?/.exec(v);
  if (!m) { return v; }
  const parts: [string, string, number][] = [
    ["yyyy", m[1], 4], ["MM", m[2], 2], ["dd", m[3], 2],
    ["HH", m[4] || "0", 2], ["mm", m[5] || "0", 2], ["ss", m[6] || "0", 2]];
  return parts.reduce((acc, p) => acc.split(p[0]).join(pad(parseInt(p[1], 10), p[2])), spec);
}

// aihtml_datagrid:format_value/3 for a raw text (the browser only
// formats what the user typed).
function format(v: string, c: Column): string {
  if (v === "") { return ""; }
  const spec = c.format;
  if (!spec) { return v; }
  const ch = spec.charAt(0);
  if ("nNcCpP".indexOf(ch) >= 0) {
    let d = parseInt(spec.slice(1), 10);
    if (isNaN(d) || d < 0) { d = 0; }
    const n = toNumber(v);
    if (n === null) { return dateFormat(v, spec); }
    if (ch === "n" || ch === "N") { return thousands(n, d); }
    if (ch === "c" || ch === "C") { return (c.currency || "") + thousands(n, d); }
    return (n * 100).toFixed(d) + "%";
  }
  return dateFormat(v, spec);
}

function display(labels: Labels, c: Column, v: string): string {
  if (v === "") { return ""; }
  if (c.type === "bool") {
    return v === "true" ? labels.yes : (v === "false" ? labels.no : v);
  }
  if (c.type === "select") {
    for (let i = 0; i < c.options.length; i++) {
      if (String(c.options[i][0]) === v) { return String(c.options[i][1]); }
    }
    return v;
  }
  return format(v, c);
}

function aggregate(a: string, nums: readonly number[]): number | null {
  if (a === "count") { return nums.length; }
  if (!nums.length) { return null; }
  const sum = nums.reduce((x, y) => x + y, 0);
  if (a === "sum") { return sum; }
  if (a === "avg") { return sum / nums.length; }
  return a === "min" ? Math.min(...nums) : Math.max(...nums);
}
function intOrFixed(v: number, d: number): string { return Number.isInteger(v) ? String(v) : v.toFixed(d); }
function statusText(a: string | null, v: number | null): string {
  if (v === null) { return ""; }
  if (a === "count") { return String(Math.trunc(v)); }
  if (a === "avg") { return v.toFixed(2); }
  return intOrFixed(v, 2);
}

function subst(p: unknown, n: number): string { return String(p).split("{0}").join(String(n)); }

// ------------------------------------------------------------------
// DOM helpers
// ------------------------------------------------------------------

function kids<E extends Element = HTMLElement>(el: Element | null, sel: string): E[] {
  if (!el) { return []; }
  return Array.prototype.filter.call(el.children, (c: Element) => c.matches(sel)) as E[];
}
function kid<E extends Element = HTMLElement>(el: Element | null, sel: string): E | null {
  return kids<E>(el, sel)[0] || null;
}
function qa<E extends Element = HTMLElement>(el: ParentNode | null, sel: string): E[] {
  return el ? Array.from(el.querySelectorAll<E>(sel)) : [];
}
function parseOne(html: string): HTMLElement {
  const t = document.createElement("template");
  t.innerHTML = html;
  return t.content.firstElementChild as HTMLElement;   // the template renders one element
}
function setStyle(node: HTMLElement, props: Partial<Record<"position" | "left" | "top" | "zIndex" | "display" | "height", string>>): void {
  Object.assign(node.style, props);
}
function isOff(node: Element): boolean { return !!node.closest(".ah-dg-row-off"); }

// The nearest element matching sel from the event target, inside root.
function hit<E extends Element = HTMLElement>(e: Event, sel: string, root: Element): E | null {
  const t = e.target;
  const m = t instanceof Element ? t.closest<E>(sel) : null;
  return m && root.contains(m) ? m : null;
}

// A plain left click follows a pager link in the page; any other (a new
// tab, a download) is the browser's.
function plainClick(e: MouseEvent): boolean {
  return e.button === 0 && !e.ctrlKey && !e.metaKey && !e.shiftKey && !e.altKey;
}

function rec(r: HTMLElement): Rec { return { el: r, key: r.getAttribute("data-key"), cells: null, stamp: 0 }; }

function cellOf(r: Rec, f: string): HTMLElement | null {
  if (!r.cells) {
    const cells = r.cells = new Map<string, HTMLElement>();
    kids(r.el, ".ah-dg-cell").forEach((c) => { cells.set(c.getAttribute("data-field") || "", c); });
  }
  return r.cells.get(f) || null;
}

function numbers(list: readonly Rec[], f: string): number[] {
  const out: number[] = [];
  list.forEach((r) => {
    const n = toNumber(raw(cellOf(r, f)));
    if (n !== null) { out.push(n); }
  });
  return out;
}

// ------------------------------------------------------------------
// Export
// ------------------------------------------------------------------

function download(blob: Blob, name: string): void {
  const url = URL.createObjectURL(blob);
  const a = document.createElement("a");
  a.href = url;
  a.download = name;
  document.body.appendChild(a);
  a.click();
  a.remove();
  setTimeout(() => { URL.revokeObjectURL(url); }, 0);
}

function csvCell(v: ExportCell | undefined): string {
  const s = v === null || v === undefined ? "" : String(v);
  return /[",\n\r]/.test(s) ? '"' + s.replace(/"/g, '""') + '"' : s;
}

// What the server sends to exportData: kept as it is when a plain value.
function exportCell(v: unknown): ExportCell {
  if (v === undefined || v === null) { return null; }
  return typeof v === "string" || typeof v === "number" || typeof v === "boolean" ? v : String(v);
}

function writeFile(name: string, fmt: string, headers: ExportCell[], rows: ExportCell[][]): Promise<void> {
  if (fmt === "csv") {
    const lines = [headers].concat(rows).map((r) => r.map(csvCell).join(","));
    download(new Blob(["﻿" + lines.join("\n")], { type: "text/csv;charset=utf-8;" }), name + ".csv");
    return Promise.resolve();
  }
  if (fmt === "xlsx") {
    return AH.vendor("xlsx").then((XLSX) => {
      const ws = XLSX.utils.aoa_to_sheet([headers].concat(rows));
      ws["!cols"] = headers.map((h, i) => {
        let w = String(h).length;
        rows.forEach((r) => { w = Math.max(w, String(r[i] === undefined ? "" : r[i]).length); });
        return { wch: w + 2 };
      });
      const wb = XLSX.utils.book_new();
      XLSX.utils.book_append_sheet(wb, ws, "Sheet1");
      const bytes: ArrayBuffer = XLSX.write(wb, { bookType: "xlsx", type: "array" });
      download(new Blob([bytes], { type: "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" }),
               name + ".xlsx");
    });
  }
  if (fmt === "pdf") {
    return AH.vendor(["jspdf", "jspdf-autotable"]).then(([jspdf, autoTable]) => {
      const doc = new jspdf.jsPDF({ orientation: headers.length > 6 ? "landscape" : "portrait", format: "a4" });
      autoTable(doc, { head: [headers], body: rows, startY: 14, styles: { fontSize: 9 },
                       headStyles: { fillColor: [66, 139, 202] } });
      download(doc.output("blob"), name + ".pdf");
    });
  }
  return Promise.reject(new Error("aihtml: unknown export format " + fmt));
}

// ------------------------------------------------------------------
// Controller
// ------------------------------------------------------------------

class DatagridController extends AH.Controller {
  // settings (data-ah-* of the root)
  #remote = false;
  #mode = "single";
  #editMode = "dblclick";
  #pageable = false;
  #headerRows = 1;
  #labels: Labels = {};
  #exportName = "data";
  #href: string | null = null;
  #pageSizes: number[] = [];
  // the view
  #sort: SortSpec[] = [];
  #filters: Record<string, string> = {};
  #groupBy: string[] = [];
  #page = 1;
  #pageSize = 10;
  #search = "";
  #collapsed = new Set<string>();
  #total = 0;
  #lastView: Rec[] | null = null;
  #stamp = 0;
  // columns and rows
  #cols: Column[] = [];
  #byField = new Map<string, Column>();
  #recs: Rec[] = [];
  // selection, focus, editing
  #sel: string[] = [];
  #anchor: string | null = null;
  #active: HTMLElement | null = null;
  #activeField: string | null = null;
  #editing: Editing | null = null;
  #resized = 0;
  #timers = new Map<string, ReturnType<typeof setTimeout>>();
  #menuOff: AbortController | null = null;
  #resizeOff: AbortController | null = null;
  #menuFloat: FloatHandle | null = null;
  #menuField: string | null = null;
  // parts (server markup)
  #body!: HTMLElement;
  #headerRow!: HTMLElement;
  #filterRow: HTMLElement | null = null;
  #pagerWrap: HTMLElement | null = null;
  #menu: HTMLElement | null = null;
  #q: HTMLElement | null = null;
  #empty: HTMLElement | null = null;

  override setup(): void {
    const el = this.element;
    this.#remote = el.hasAttribute("data-ah-remote");
    this.#mode = el.getAttribute("data-ah-selection") || "single";
    this.#editMode = el.getAttribute("data-ah-edit-mode") || "dblclick";
    this.#pageable = el.hasAttribute("data-ah-pageable");
    this.#headerRows = parseInt(el.getAttribute("data-ah-header-rows") || "1", 10);
    this.#labels = parseLabels(el.getAttribute("data-ah-labels"));
    this.#exportName = el.getAttribute("data-ah-export-name") || "data";
    this.#href = el.getAttribute("data-ah-href");
    this.#sort = parseSort(el.getAttribute("data-sort"));
    this.#filters = parseFilters(el.getAttribute("data-filter"));
    this.#groupBy = parseStrings(el.getAttribute("data-group-by"));
    this.#page = parseInt(el.getAttribute("data-page") || "1", 10) || 1;
    this.#pageSize = parseInt(el.getAttribute("data-page-size") || "10", 10) || 10;
    this.#search = "";
    this.#collapsed = new Set();
    this.#sel = split(el.getAttribute("data-ah-value")).filter(Boolean);
    this.#anchor = null;
    this.#active = null;
    this.#activeField = null;
    this.#editing = null;
    this.#timers = new Map();
    this.#lastView = null;
    // the server always renders the body and the header row
    this.#body = el.querySelector<HTMLElement>(".ah-dg-body") as HTMLElement;
    this.#headerRow = el.querySelector<HTMLElement>(".ah-dg-header-row") as HTMLElement;
    this.#filterRow = el.querySelector<HTMLElement>(".ah-dg-header-filter-row");
    this.#pagerWrap = el.querySelector<HTMLElement>(".ah-dg-pager-wrap");
    this.#menu = kid(el, ".ah-dg-column-menu");
    this.#q = kid(el, ".ah-dg-query");
    this.#pageSizes = qa<HTMLOptionElement>(el, ".ah-dg-pager-size-select option").map((o) => parseInt(o.value, 10));
    if (!this.#pageSizes.length) { this.#pageSizes = [10, 20, 50, 100]; }
    this.#empty = kid(this.#body, ".ah-dg-empty-message");
    this.#readCols();
    this.#readRecs();
    this.#total = this.#remote
      ? parseInt(el.getAttribute("aria-rowcount") || "0", 10) - this.#headerRows : this.#recs.length;
    el.removeAttribute("tabindex");

    // the body scrolls the header and the status bar along (scroll does
    // not bubble: captured on the root)
    this.listen(el, "scroll", (e) => {
      const wrap = el.querySelector(".ah-dg-body-wrap");
      if (!wrap || e.target !== wrap) { return; }
      const x = wrap.scrollLeft;
      qa(el, ".ah-dg-header-wrap, .ah-dg-statusbar-wrap").forEach((w) => { w.scrollLeft = x; });
    }, { capture: true });

    // header: resize
    this.delegate("pointerdown", ".ah-dg-resize-handle", (e, h) => { this.#startResize(e, h); });

    // clicks, the innermost target first, each handler able to stop
    // the ones further out
    this.listen(el, "click", (e) => { this.#click(e); });
    this.delegate("dblclick", ".ah-dg-row", (e, row) => {
      if (row.closest(".ah-dg") !== el || this.#editing) { return; }
      const cell = hit(e, ".ah-dg-cell", row);
      this.#emit("ah:row-dblclick", { key: row.getAttribute("data-key"),
                                      field: cell ? cell.getAttribute("data-field") : null });
      if (cell && this.#editMode === "dblclick" && cell.classList.contains("ah-dg-cell-editable")) {
        this.#beginEdit(cell);
      }
    });

    // select all, pager size
    this.delegate<Event, HTMLInputElement>("change", ".ah-dg-select-all", (_e, box) => { this.#selectAll(box.checked); });
    this.delegate<Event, HTMLSelectElement>("change", ".ah-dg-pager-size-select", (_e, sel) => {
      this.#pageSize = parseInt(sel.value, 10) || this.#pageSize;
      this.#page = 1;
      this.#view();
      this.#emit("ah:page", { value: 1 });
    });

    // filter row, search box (debounced)
    this.delegate<Event, HTMLInputElement>("input", ".ah-dg-filter-input", (_e, input) => {
      const f = input.getAttribute("data-field") || "";
      this.#debounce("f:" + f, this.#remote ? 300 : 200, () => { this.#setFilter(f, input.value); });
    });
    this.delegate<Event, HTMLInputElement>("input", ".ah-dg-search-input", (_e, input) => {
      this.#debounce("search", this.#remote ? 300 : 200, () => {
        this.#search = input.value;
        this.#page = 1;
        this.#view();
        this.#emit("ah:filter", { field: "", value: input.value });
      });
    });

    // keyboard, focus ring
    this.listen(el, "keydown", (e) => {
      if (this.#menu && this.#menu.contains(e.target as Node | null)) { this.#menuKey(e); return; }
      this.#keydown(e);
    });
    this.delegate("focusin", ".ah-dg-cell, .ah-dg-header-cell, .ah-dg-group-title", (_e, cell) => {
      if (cell === this.#active) { cell.classList.add("ah-dg-cell-focused"); }
    });
    this.listen(el, "focusout", (e) => {
      const to = e.relatedTarget as Node | null;
      if (this.#active && !(to && to !== el && el.contains(to))) { this.#active.classList.remove("ah-dg-cell-focused"); }
    });
    if (this.#menu) {
      this.delegate("click", ".ah-dg-column-menu-item", (e, item) => {
        e.stopPropagation();
        this.#menuAction(item);
      }, this.#menu);
    }
    if (this.#q) {
      this.listen(this.#q, "ah:error", () => { this.#loading(false); });
    }

    // inner controls report to the grid, not as the grid's own change /
    // input; registered last so the grid's own handlers above still run,
    // and stopImmediatePropagation also keeps them from listeners added
    // to the root later
    const inner = (e: Event): void => {
      const t = e.target;
      if (t !== el && t instanceof Element && t.matches("input, select, textarea")) {
        e.stopImmediatePropagation();
      }
    };
    this.listen(el, "change", inner);
    this.listen(el, "input", inner);

    this.#layout();
    this.#paintSelection();
    if (this.#remote) {
      this.#paintHeader();
      this.#writeState();
      this.#resetActive(false);
    } else {
      this.#view();
    }
  }

  override teardown(): void {
    this.#timers.forEach((t) => { clearTimeout(t); });
    if (this.#resizeOff) { this.#resizeOff.abort(); this.#resizeOff = null; }
    if (this.#menuOff) { this.#menuOff.abort(); this.#menuOff = null; }
    if (this.#menuFloat) { this.#menuFloat.stop(); this.#menuFloat = null; }
    if (this.#editing) { this.#editing.off.abort(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: unknown): void {
    this.#sel = split(Array.isArray(v) ? v : v == null ? "" : String(v)).filter(Boolean);
    this.#writeValue(false);
  }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  sort(field: unknown, dir?: unknown): void { this.#setSort(String(field), dir ? String(dir) : null); }
  filter(field: unknown, v?: unknown): void { this.#setFilter(String(field), v ? String(v) : ""); }
  search(v: unknown): void {
    this.#search = v ? String(v) : "";
    qa<HTMLInputElement>(this.element, ".ah-dg-search-input").forEach((i) => { i.value = this.#search; });
    this.#page = 1;
    this.#view();
  }
  goToPage(p: unknown): void { this.#goToPage(parseInt(String(p), 10) || 1); }
  groupBy(fields: unknown): void {
    if (!this.#remote) { this.#setGroupBy((Array.isArray(fields) ? fields : []).map(String)); }
  }
  showColumn(f: unknown): void { const c = this.#byField.get(String(f)); if (c) { this.#setHidden(c, false); } }
  hideColumn(f: unknown): void { const c = this.#byField.get(String(f)); if (c) { this.#setHidden(c, true); } }
  pinColumn(f: unknown, pinned?: unknown): void {
    const c = this.#byField.get(String(f));
    if (c) { c.pinned = pinned !== false; this.#layout(); }
  }
  setColumnWidth(f: unknown, w: unknown): void {
    const c = this.#byField.get(String(f));
    if (c) { c.width = Math.max(c.minWidth, parseInt(String(w), 10) || c.width); this.#layout(); }
  }
  exportData(fmt: unknown, headers?: unknown, rows?: unknown): Promise<void> {
    if (headers) {
      const hs = (Array.isArray(headers) ? headers : []).map(exportCell);
      const rs = (Array.isArray(rows) ? rows : []).map((r: unknown) => (Array.isArray(r) ? r : []).map(exportCell));
      return writeFile(this.#exportName || "data", String(fmt), hs, rs);
    }
    return this.#export(String(fmt));
  }
  refresh(): void {
    if (this.#remote) { this.#query(null); } else { this.#view(); }
  }
  rowsLoaded(total: unknown, page?: unknown): void {
    const el = this.element;
    this.#total = parseInt(String(total), 10) || 0;
    if (page) { this.#page = parseInt(String(page), 10) || this.#page; }
    const pages = Math.ceil(this.#total / this.#pageSize);
    if (this.#pageable && pages > 0 && this.#page > pages) {
      this.#page = pages;
      this.#writeState();
      this.#query(null);
      return;
    }
    this.#loading(false);
    el.setAttribute("data-ah-loaded", "true");
    el.setAttribute("aria-rowcount", String(this.#total + this.#headerRows));
    this.#adoptRows();
    this.#paintRows(this.#pageable ? (this.#page - 1) * this.#pageSize : 0);
    this.#paintSelection();
    const focusLost = document.activeElement === document.body;
    this.#resetActive(focusLost);
  }
  rowUpdated(rowId: unknown): void {
    this.#adoptRows();
    if (this.#remote) {
      this.#paintRows(this.#pageable ? (this.#page - 1) * this.#pageSize : 0);
      this.#paintSelection();
    } else {
      this.#viewLocal();
    }
    const row = document.getElementById(String(rowId));
    if (row && this.#active && !this.#active.isConnected) {
      this.#activate(this.#cellAt(row, this.#activeField), false);
    }
  }

  // ------------------------------------------------------------------
  // State
  // ------------------------------------------------------------------

  #readCols(): void {
    this.#cols = [];
    this.#byField = new Map();
    kids(this.#headerRow, ".ah-dg-header-cell").forEach((h) => {
      const f = h.getAttribute("data-field") || "";
      const content = kid(h, ".ah-dg-header-cell-content");
      const c: Column = {
        field: f, head: h, check: f === CHECK,
        title: content ? (content.textContent || "") : "",
        type: h.getAttribute("data-type") || "text",
        editable: h.getAttribute("data-editable") === "true",
        sortable: f !== CHECK && h.getAttribute("data-sortable") !== "false",
        groupable: f !== CHECK && h.getAttribute("data-groupable") !== "false",
        pinned: h.getAttribute("data-pinned") === "true" ||
          (f === CHECK && h.style.position === "sticky"),
        hidden: h.classList.contains("ah-dg-col-hidden"),
        width: parseInt(h.style.width, 10) || h.offsetWidth || 100,
        minWidth: parseInt(h.getAttribute("data-min-width") || "40", 10),
        format: h.getAttribute("data-format"),
        currency: h.getAttribute("data-currency") || "",
        options: parseOptions(h.getAttribute("data-options")),
        aggs: (h.getAttribute("data-aggs") || "").split(",").filter(Boolean)
      };
      this.#cols.push(c);
      this.#byField.set(f, c);
    });
  }

  #readRecs(): void {
    const rows = kids(this.#body, ".ah-dg-row");
    if (rows.length && rows[0].hasAttribute("data-i")) {
      rows.sort((a, b) =>
        parseInt(a.getAttribute("data-i") || "", 10) - parseInt(b.getAttribute("data-i") || "", 10));
    }
    this.#recs = rows.map(rec);
  }

  #col(field: string | null): Column | undefined {
    return field === null ? undefined : this.#byField.get(field);
  }
  #visibleCols(): Column[] { return this.#cols.filter((c) => !c.hidden); }
  #dataCols(): Column[] { return this.#cols.filter((c) => !c.check); }

  #writeState(): void {
    const el = this.element;
    el.setAttribute("data-sort", JSON.stringify(this.#sort));
    el.setAttribute("data-filter", JSON.stringify(this.#filters));
    el.setAttribute("data-page", String(this.#page));
    el.setAttribute("data-page-size", String(this.#pageSize));
    el.setAttribute("data-group-by", JSON.stringify(this.#groupBy));
  }

  // Fire a component event with its details as data-* of the root, so
  // a postback bound on the root receives them in Event.data.
  #emit(name: string, detail: GridEvent = {}): void {
    const el = this.element;
    DETAIL.forEach((k) => {
      const v = detail[k];
      if (v !== undefined && v !== null) { el.setAttribute("data-" + k, String(v)); }
    });
    try {
      this.fire<GridEvent>(name, detail);
    } finally {
      DETAIL.forEach((k) => { el.removeAttribute("data-" + k); });
    }
  }

  #debounce(key: string, ms: number, f: () => void): void {
    clearTimeout(this.#timers.get(key));
    this.#timers.set(key, setTimeout(f, ms));
  }

  // ------------------------------------------------------------------
  // The view
  // ------------------------------------------------------------------

  #view(): void {
    this.#writeState();
    this.#paintHeader();
    if (this.#remote) {
      this.#query(null);
    } else {
      this.#viewLocal();
    }
  }

  #matches(r: Rec, filters: readonly [string, string][], q: string): boolean {
    for (let i = 0; i < filters.length; i++) {
      const c = cellOf(r, filters[i][0]);
      if (!c) { continue; }
      const n = filters[i][1];
      if (raw(c).toLowerCase().indexOf(n) < 0 && text(c).toLowerCase().indexOf(n) < 0) { return false; }
    }
    if (!q) { return true; }
    return this.#dataCols().some((col) => {
      const c = cellOf(r, col.field);
      return !!c && (raw(c).toLowerCase().indexOf(q) >= 0 || text(c).toLowerCase().indexOf(q) >= 0);
    });
  }

  #sortRecs(list: readonly Rec[]): Rec[] {
    const sort = this.#sort;
    if (!sort.length) { return list.slice(); }
    const keyed = list.map((r, i) => ({ r, i, k: sort.map((sd) => raw(cellOf(r, sd[0]))) }));
    keyed.sort((a, b) => {
      for (let j = 0; j < sort.length; j++) {
        const x = compare(a.k[j], b.k[j]);
        if (x) { return sort[j][1] === "desc" ? -x : x; }
      }
      return a.i - b.i;
    });
    return keyed.map((x) => x.r);
  }

  #groupRecs(list: readonly Rec[], fields: readonly string[], level: number, path: readonly string[],
             out: Item[]): void {
    if (!fields.length) {
      list.forEach((r) => { out.push({ row: r }); });
      return;
    }
    const f = fields[0], keys: string[] = [], by = new Map<string, Rec[]>();
    list.forEach((r) => {
      const k = raw(cellOf(r, f));
      let members = by.get(k);
      if (!members) { members = []; by.set(k, members); keys.push(k); }
      members.push(r);
    });
    keys.sort(compare);
    keys.forEach((k) => {
      const p = path.concat([f + ":" + k]);
      const id = p.join("|");
      const members = by.get(k) as Rec[];
      const collapsed = this.#collapsed.has(id);
      out.push({ group: { id, level, title: text(cellOf(members[0], f)),
                          count: members.length, recs: members, collapsed } });
      if (!collapsed) { this.#groupRecs(members, fields.slice(1), level + 1, p, out); }
    });
  }

  #groupEl(g: Group): HTMLElement {
    const aggs: { label: string; text: string }[] = [];
    this.#visibleCols().forEach((c) => {
      if (!c.aggs.length) { return; }
      const nums = numbers(g.recs, c.field);
      const parts: string[] = [];
      c.aggs.forEach((a) => {
        const v = aggregate(a, nums);
        if (v !== null) { parts.push(a + "=" + intOrFixed(v, 1)); }
      });
      if (parts.length) { aggs.push({ label: c.title, text: parts.join(", ") }); }
    });
    return parseOne(AH.tpl.datagrid_group_row({
      id: g.id, level: g.level, aria_level: g.level + 1,
      expanded: g.collapsed ? "false" : "true", open: !g.collapsed,
      indent: g.level * 20, colspan: this.#visibleCols().length,
      title: g.title, count: g.count, has_aggs: aggs.length > 0, aggs
    }));
  }

  #statusbar(list: readonly Rec[]): void {
    const bar = this.element.querySelector(".ah-dg-statusbar");
    if (!bar) { return; }
    this.#cols.forEach((c) => {
      if (!c.aggs.length) { return; }
      const nums = numbers(list, c.field);
      qa(bar, ".ah-dg-statusbar-cell").filter((cell) => cell.getAttribute("data-field") === c.field)
        .forEach((cell) => {
          qa(cell, ".ah-dg-statusbar-item").forEach((item) => {
            const a = item.getAttribute("data-agg");
            kids(item, ".ah-dg-statusbar-value").forEach((v) => {
              v.textContent = statusText(a, aggregate(a || "", nums));
            });
          });
        });
    });
  }

  #viewLocal(): void {
    const el = this.element, body = this.#body;
    const active = document.activeElement as HTMLElement | null;
    const hadFocus = !!active && active !== body && body.contains(active);
    const activeGroupRow = hadFocus && active ? active.closest(".ah-dg-group-row") : null;
    const activeGroup = activeGroupRow ? activeGroupRow.getAttribute("data-group-id") : null;
    const filters = Object.keys(this.#filters)
      .filter((f) => this.#filters[f] !== "" && this.#byField.has(f))
      .map((f): [string, string] => [f, String(this.#filters[f]).toLowerCase()]);
    const q = this.#search.toLowerCase();
    const kept: Rec[] = [];
    this.#recs.forEach((r) => { if (this.#matches(r, filters, q)) { kept.push(r); } });
    this.#statusbar(kept);
    const sorted = this.#sortRecs(kept);
    this.#lastView = sorted;
    const flat: Item[] = [];
    if (this.#groupBy.length) { this.#groupRecs(sorted, this.#groupBy, 0, [], flat); }
    else { sorted.forEach((r) => { flat.push({ row: r }); }); }
    const total = flat.length;
    const pages = Math.max(1, Math.ceil(total / this.#pageSize));
    this.#page = this.#pageable ? Math.min(Math.max(1, this.#page), pages) : 1;
    const lo = this.#pageable ? (this.#page - 1) * this.#pageSize : 0;
    const hi = this.#pageable ? lo + this.#pageSize : total;
    kids(body, ".ah-dg-group-row").forEach((g) => { g.remove(); });
    const frag = document.createDocumentFragment();
    const stamp = ++this.#stamp;
    let focusGroup: HTMLElement | null = null;
    flat.forEach((it, i) => {
      const on = i >= lo && i < hi;
      if (it.group) {
        if (on) {
          const g = this.#groupEl(it.group);
          if (activeGroup === it.group.id) { focusGroup = g; }
          frag.appendChild(g);
        }
        return;
      }
      it.row.stamp = stamp;
      it.row.el.classList.toggle("ah-dg-row-off", !on);
      frag.appendChild(it.row.el);
    });
    this.#recs.forEach((r) => {
      if (r.stamp !== stamp) {
        r.el.classList.add("ah-dg-row-off");
        frag.appendChild(r.el);
      }
    });
    body.insertBefore(frag, this.#empty);
    this.#total = total;
    this.#paintRows(lo);
    this.#renderPager();
    this.#paintSelection();
    if (hadFocus && active) {
      if (focusGroup) { this.#activate(kid(focusGroup, ".ah-dg-group-title"), true); }
      else if (active.isConnected && !isOff(active)) { active.focus(); }
      else { this.#resetActive(true); }
    } else {
      this.#resetActive(false);
    }
    el.setAttribute("aria-rowcount", String(total + this.#headerRows));
  }

  // Stripes and aria-rowindex of the rows on the page; the empty message.
  #paintRows(offset: number): void {
    let idx = 0, ri = this.#headerRows + offset + 1;
    kids(this.#body, ".ah-dg-row, .ah-dg-group-row").forEach((r) => {
      if (r.classList.contains("ah-dg-row-off")) { r.removeAttribute("aria-rowindex"); return; }
      if (r.classList.contains("ah-dg-row")) {
        r.classList.toggle("ah-dg-row-even", idx % 2 === 0);
        r.classList.toggle("ah-dg-row-odd", idx % 2 === 1);
        r.setAttribute("aria-rowindex", String(ri++));
      }
      idx++;
    });
    const empty = idx === 0;
    this.#body.classList.toggle("ah-dg-body-empty", empty);
    if (this.#empty) { this.#empty.hidden = !empty; }
  }

  #pagerView(): PagerView {
    const L = this.#labels, total = this.#total || 0, ps = this.#pageSize;
    const pages = Math.ceil(total / ps);
    const page = Math.max(1, Math.min(this.#page, Math.max(1, pages)));
    const end = Math.min(pages, Math.max(1, page - Math.floor(PAGER_BUTTONS / 2)) + PAGER_BUTTONS - 1);
    const start = Math.max(1, end - PAGER_BUTTONS + 1);
    const list: PagerView["pages"] = [];
    for (let p = start; pages > 0 && p <= end; p++) { list.push({ page: p, active: p === page }); }
    const sizes = this.#pageSizes.slice();
    if (sizes.indexOf(ps) < 0) { sizes.push(ps); }
    sizes.sort((a, b) => a - b);
    return {
      label: L.pages, info: subst(L.total, total),
      prev: Math.max(1, page - 1), next: Math.min(Math.max(1, pages), page + 1), last: Math.max(1, pages),
      at_start: page <= 1, at_end: page >= pages,
      first_label: L.first_page, prev_label: L.prev_page, next_label: L.next_page, last_label: L.last_page,
      pages: list, size_label: L.page_size,
      sizes: sizes.map((n) => ({ size: n, text: subst(L.per_page, n), selected: n === ps }))
    };
  }

  // The grid's href with the view filled in but {page}; mirrors
  // aihtml_datagrid:link_template/4.
  #linkTemplate(href: string): string {
    const sort = this.#sort.map((sd) => encodeURIComponent(sd[0]) + ":" + sd[1]).join(",");
    return href.replace(/\{size\}/g, String(this.#pageSize)).replace(/\{sort\}/g, sort)
      .replace(/\{search\}/g, encodeURIComponent(this.#search || ""));
  }

  #renderPager(): void {
    const wrap = this.#pagerWrap;
    if (!this.#pageable || !wrap) { return; }
    const focused = document.activeElement;
    const refocus = focused && focused !== wrap && wrap.contains(focused) ?
      (focused.getAttribute("data-page") ? "[aria-current=page]" : "select") : null;
    const v = this.#pagerView();
    wrap.innerHTML = AH.tpl.datagrid_pager(this.#href ? withLinks(v, this.#linkTemplate(this.#href)) : v);
    if (refocus) {
      const again = wrap.querySelector<HTMLElement>(refocus);
      if (again) { again.focus(); }
    }
  }

  #paintHeader(): void {
    this.#cols.forEach((c) => {
      if (c.check || !c.sortable) { return; }
      let i = -1;
      this.#sort.forEach((sd, j) => { if (sd[0] === c.field) { i = j; } });
      const dir = i >= 0 ? this.#sort[i][1] : null;
      c.head.classList.toggle("ah-dg-header-cell-sorted", !!dir);
      c.head.setAttribute("aria-sort", dir === "asc" ? "ascending" : (dir === "desc" ? "descending" : "none"));
      const icon = kid(c.head, ".ah-dg-header-sort-icon");
      if (!icon) { return; }
      icon.textContent = dir === "asc" ? "▲" : (dir === "desc" ? "▼" : "");
      if (dir && this.#sort.length > 1) {
        const badge = document.createElement("span");
        badge.className = "ah-dg-header-sort-badge";
        badge.textContent = String(i + 1);
        icon.appendChild(badge);
      }
    });
    if (this.#filterRow) {
      kids(this.#filterRow, ".ah-dg-filter-cell").forEach((cell) => {
        const f = cell.getAttribute("data-field") || "";
        cell.classList.toggle("ah-dg-filter-cell-active", !!this.#filters[f]);
        const input = kid<HTMLInputElement>(cell, ".ah-dg-filter-input");
        if (input && input !== document.activeElement && input.value !== (this.#filters[f] || "")) {
          input.value = this.#filters[f] || "";
        }
      });
    }
  }

  // ------------------------------------------------------------------
  // Remote mode
  // ------------------------------------------------------------------

  #query(exportFormat: string | null): void {
    const q = this.#q;
    if (!q) { return; }
    q.setAttribute("data-render", this.element.getAttribute("data-render") || "");
    q.setAttribute("data-sort", JSON.stringify(this.#sort));
    q.setAttribute("data-filter", JSON.stringify(this.#filters));
    q.setAttribute("data-search", this.#search);
    q.setAttribute("data-page", String(this.#page));
    q.setAttribute("data-page-size", String(this.#pageSize));
    q.setAttribute("data-header-rows", String(this.#headerRows));
    if (exportFormat) {
      q.setAttribute("data-export", exportFormat);
    } else {
      this.#loading(true);
    }
    try {
      this.fire("ah:query", null, q);
    } finally {
      q.removeAttribute("data-export");
    }
  }

  #loading(on: boolean): void {
    const el = this.element;
    kids(el, ".ah-dg-loading-overlay").forEach((o) => { o.style.display = on ? "flex" : "none"; });
    el.setAttribute("aria-busy", on ? "true" : "false");
  }

  // After the server morphed rows in (rowsLoaded, rowUpdated).
  #adoptRows(): void {
    if (this.#remote) {
      this.#recs = kids(this.#body, ".ah-dg-row").map(rec);
    } else {
      const byKey = new Map<string | null, HTMLElement>();
      kids(this.#body, ".ah-dg-row").forEach((r) => { byKey.set(r.getAttribute("data-key"), r); });
      this.#recs.forEach((r) => {
        const now = byKey.get(r.key);
        if (now && now !== r.el) { r.el = now; }
        r.cells = null;
      });
    }
    this.#empty = kid(this.#body, ".ah-dg-empty-message");
    this.#layout(this.#body);
  }

  // ------------------------------------------------------------------
  // Columns: widths, hidden, pinned
  // ------------------------------------------------------------------

  #layout(scope?: Element): void {
    const el = this.element;
    let left = 0, lastPinned: string | null = null;
    const offsets: Record<string, number> = {};
    this.#visibleCols().forEach((c) => {
      if (c.pinned) { offsets[c.field] = left; left += c.width; lastPinned = c.field; }
    });
    qa(scope || el, CELLS).forEach((cell) => {
      const c = this.#col(cell.getAttribute("data-field"));
      if (!c || cell.closest(".ah-dg") !== el) { return; }
      cell.style.width = c.width + "px";
      cell.classList.toggle("ah-dg-col-hidden", c.hidden);
      cell.classList.toggle("ah-dg-cell-pinned-last", c.field === lastPinned && !c.hidden);
      if (c.pinned && !c.hidden) {
        setStyle(cell, { position: "sticky", left: offsets[c.field] + "px", zIndex: "2" });
      } else if (cell.style.position === "sticky") {
        setStyle(cell, { position: "", left: "", zIndex: "" });
      }
    });
    el.setAttribute("aria-colcount", String(this.#visibleCols().length));
  }

  #setHidden(c: Column, hidden: boolean): void {
    if (hidden && this.#visibleCols().length <= 1) { return; }
    c.hidden = hidden;
    this.#layout();
    if (!this.#remote && this.#groupBy.length) { this.#viewLocal(); }
    if (this.#active && this.#active.classList.contains("ah-dg-col-hidden")) { this.#resetActive(false); }
  }

  // ------------------------------------------------------------------
  // Selection
  // ------------------------------------------------------------------

  #pageRows(): HTMLElement[] {
    return kids(this.#body, ".ah-dg-row").filter((r) => !r.classList.contains("ah-dg-row-off"));
  }

  #paintSelection(): void {
    const set = new Set(this.#sel);
    kids(this.#body, ".ah-dg-row").forEach((r) => {
      const on = set.has(r.getAttribute("data-key") as string);
      r.classList.toggle("ah-dg-row-selected", on);
      if (this.#mode !== "none") { r.setAttribute("aria-selected", on ? "true" : "false"); }
      const cb = kid<HTMLInputElement>(kid(r, ".ah-dg-cell-checkbox"), "input");
      if (cb) { cb.checked = on; }
    });
    const all = this.element.querySelector<HTMLInputElement>(".ah-dg-select-all");
    if (all) {
      const rows = this.#pageRows();
      const n = rows.filter((r) => set.has(r.getAttribute("data-key") as string)).length;
      all.checked = rows.length > 0 && n === rows.length;
      all.indeterminate = n > 0 && n < rows.length;
    }
  }

  #writeValue(user: boolean): void {
    const el = this.element;
    const before = el.getAttribute("data-ah-value") || "";
    const v = join(this.#sel);
    el.setAttribute("data-ah-value", v);
    kids<HTMLInputElement>(el, "input[type=hidden][data-ah-input]").forEach((i) => { i.value = v; });
    this.#paintSelection();
    if (user && v !== before) { this.fire("change"); }
  }

  #toggleKey(key: string): void {
    const i = this.#sel.indexOf(key);
    if (i >= 0) { this.#sel.splice(i, 1); } else { this.#sel.push(key); }
  }

  #selectRow(row: HTMLElement | null, e: MouseEvent | null, toggle: boolean): void {
    if (this.#mode === "none" || !row) { return; }
    const key = row.getAttribute("data-key") as string;   // every row has its key
    if (this.#mode === "single") {
      this.#sel = [key];
    } else if (this.#mode === "checkbox" || toggle || (e && (e.ctrlKey || e.metaKey))) {
      this.#toggleKey(key);
    } else if (e && e.shiftKey && this.#anchor !== null) {
      const keys = this.#pageRows().map((r) => r.getAttribute("data-key") as string);
      const a = keys.indexOf(this.#anchor), b = keys.indexOf(key);
      if (a < 0) { this.#sel = [key]; } else { this.#sel = keys.slice(Math.min(a, b), Math.max(a, b) + 1); }
    } else {
      this.#sel = [key];
    }
    if (!(e && e.shiftKey)) { this.#anchor = key; }
    this.#writeValue(true);
  }

  #selectAll(on?: boolean): void {
    const keys = this.#pageRows().map((r) => r.getAttribute("data-key") as string);
    const turnOn = on === undefined ? !keys.every((k) => this.#sel.indexOf(k) >= 0) : on;
    keys.forEach((k) => {
      const i = this.#sel.indexOf(k);
      if (turnOn && i < 0) { this.#sel.push(k); }
      if (!turnOn && i >= 0) { this.#sel.splice(i, 1); }
    });
    this.#writeValue(true);
  }

  // ------------------------------------------------------------------
  // Keyboard: an active cell with the only tabindex="0" (roving)
  // ------------------------------------------------------------------

  #navRows(): HTMLElement[] {
    const rows = [this.#headerRow];
    kids(this.#body, ".ah-dg-row, .ah-dg-group-row").forEach((r) => {
      if (!r.classList.contains("ah-dg-row-off")) { rows.push(r); }
    });
    return rows;
  }

  #cellAt(row: Element | null | undefined, field: string | null): HTMLElement | null {
    if (!row) { return null; }
    if (row.classList.contains("ah-dg-group-row")) { return kid(row, ".ah-dg-group-title"); }
    const sel = row === this.#headerRow ? ".ah-dg-header-cell" : ".ah-dg-cell";
    return kids(row, sel).filter((c) => c.getAttribute("data-field") === field)[0] || null;
  }

  #activate(cell: HTMLElement | null, focus: boolean): void {
    if (!cell) { return; }
    if (this.#active && this.#active !== cell) {
      this.#active.removeAttribute("tabindex");
      this.#active.classList.remove("ah-dg-cell-focused");
    }
    this.#active = cell;
    cell.setAttribute("tabindex", "0");
    const f = cell.getAttribute("data-field");
    if (f) { this.#activeField = f; }
    if (focus) {
      cell.classList.add("ah-dg-cell-focused");
      cell.focus({ preventScroll: false });
    }
  }

  // Keep a tab stop: the active cell, else the header cell of its column.
  #resetActive(focus: boolean): void {
    const a = this.#active;
    if (a && a.isConnected && !isOff(a) && !a.classList.contains("ah-dg-col-hidden")) {
      if (focus) { this.#activate(a, true); }
      return;
    }
    const cols = this.#visibleCols();
    let c = this.#col(this.#activeField);
    if (!c || c.hidden) { c = cols[0]; }
    if (c) { this.#activate(c.head, focus); }
  }

  #move(cell: HTMLElement, dr: number, dc: number, e: KeyboardEvent): void {
    const row = cell.closest<HTMLElement>(".ah-dg-row, .ah-dg-group-row, .ah-dg-header-row");
    const rows = this.#navRows();
    const cols = this.#visibleCols();
    const f = cell.getAttribute("data-field") || this.#activeField;
    let ci = cols.map((c) => c.field).indexOf(f as string);
    if (ci < 0) { ci = 0; }
    let r = row ? rows.indexOf(row) : -1;
    const key = e.key;
    if (key === "Home" && !(e.ctrlKey || e.metaKey)) { ci = 0; }
    else if (key === "End" && !(e.ctrlKey || e.metaKey)) { ci = cols.length - 1; }
    else if (key === "Home") { r = 0; }
    else if (key === "End") { r = rows.length - 1; }
    else { r += dr; ci += dc; }
    r = Math.max(0, Math.min(rows.length - 1, r));
    ci = Math.max(0, Math.min(cols.length - 1, ci));
    this.#activate(this.#cellAt(rows[r], cols[ci].field), true);
  }

  #keydown(e: KeyboardEvent): void {
    if (this.#editing) { return; }
    const el = this.element;
    const cell = e.target;
    if (!(cell instanceof HTMLElement) ||
        !cell.matches(".ah-dg-cell, .ah-dg-header-cell, .ah-dg-group-title") ||
        cell.closest(".ah-dg") !== el) { return; }
    const header = cell.classList.contains("ah-dg-header-cell");
    const group = cell.closest<HTMLElement>(".ah-dg-group-row");
    const row = cell.closest<HTMLElement>(".ah-dg-row");
    const field = cell.getAttribute("data-field");
    const c = field ? this.#byField.get(field) : undefined;
    const multi = this.#mode === "multi" || this.#mode === "checkbox";
    switch (e.key) {
      case "ArrowUp": this.#move(cell, -1, 0, e); break;
      case "ArrowDown":
        if (header && e.altKey) { this.#openMenu(field); break; }
        this.#move(cell, 1, 0, e);
        break;
      case "ArrowLeft":
        if (group) { this.#toggleGroup(group, false); break; }
        this.#move(cell, 0, -1, e);
        break;
      case "ArrowRight":
        if (group) { this.#toggleGroup(group, true); break; }
        this.#move(cell, 0, 1, e);
        break;
      case "Home": case "End": this.#move(cell, 0, 0, e); break;
      case "PageUp": case "PageDown":
        if (this.#pageable) {
          this.#goToPage(this.#page + (e.key === "PageDown" ? 1 : -1));
          const rows = this.#navRows();
          this.#activate(this.#cellAt(rows[Math.min(1, rows.length - 1)], this.#activeField), true);
        } else {
          this.#move(cell, e.key === "PageDown" ? 10 : -10, 0, e);
        }
        break;
      case "Enter":
        if (header) { if (c && c.check) { this.#selectAll(); } else { this.#headerSort(c, e.shiftKey); } break; }
        if (group) { this.#toggleGroup(group); break; }
        if (c && c.editable) { this.#beginEdit(cell); break; }
        this.#selectRow(row, null, multi);
        break;
      case "F2":
        if (c && c.editable && row) { this.#beginEdit(cell); }
        break;
      case " ":
        if (header) { if (c && c.check) { this.#selectAll(); } break; }
        if (group) { this.#toggleGroup(group); break; }
        this.#selectRow(row, null, multi);
        break;
      case "ContextMenu":
        if (header && field !== CHECK) { this.#openMenu(field); break; }
        return;
      case "a": case "A":
        if ((e.ctrlKey || e.metaKey) && multi) { this.#selectAll(true); break; }
        return;
      default:
        return;
    }
    e.preventDefault();
  }

  // ------------------------------------------------------------------
  // Clicks
  // ------------------------------------------------------------------

  #click(e: MouseEvent): void {
    const el = this.element;
    let m: HTMLElement | null;
    if ((m = hit(e, ".ah-dg-column-menu-btn", el))) {
      e.preventDefault();
      e.stopPropagation();
      const f = m.getAttribute("data-field");
      if (this.#menuField === f) { this.#closeMenu(true); } else { this.#openMenu(f); }
    } else if ((m = hit(e, ".ah-dg-header-cell", el))) {
      if (m.closest(".ah-dg") !== el) { return; }
      if (hit(e, ".ah-dg-resize-handle, .ah-dg-column-menu-btn, input", m)) { return; }
      if (this.#resized && Date.now() - this.#resized < 300) { return; }
      this.#activate(m, true);
      this.#headerSort(this.#col(m.getAttribute("data-field")), e.shiftKey);
    } else if ((m = hit(e, ".ah-dg-row-checkbox", el))) {
      e.stopPropagation();
      const row = m.closest(".ah-dg-row") as HTMLElement;   // a row box sits in its row
      const key = row.getAttribute("data-key") as string;
      if ((this.#sel.indexOf(key) >= 0) !== (m as HTMLInputElement).checked) { this.#toggleKey(key); }
      this.#anchor = key;
      this.#writeValue(true);
    } else if ((m = hit(e, ".ah-dg-command-btn", el))) {
      e.stopPropagation();
      const crow = m.closest(".ah-dg-row");
      this.#emit("ah:command", { key: crow && crow.getAttribute("data-key"),
                                 field: m.getAttribute("data-field"),
                                 name: m.getAttribute("data-command") });
    } else if ((m = hit(e, ".ah-dg-row", el))) {
      if (m.closest(".ah-dg") !== el || this.#editing) { return; }
      const cell = hit(e, ".ah-dg-cell", m);
      const field = cell ? cell.getAttribute("data-field") : null;
      if (cell && !hit(e, "a, button, input, select, textarea", m)) {
        this.#activate(cell, true);
      }
      this.#emit("ah:row-click", { key: m.getAttribute("data-key"), field });
      if (!hit(e, "a", m)) { this.#selectRow(m, e, false); }
      if (cell && this.#editMode === "click" && cell.classList.contains("ah-dg-cell-editable")) {
        this.#beginEdit(cell);
      }
    } else if ((m = hit(e, ".ah-dg-group-row", el))) {
      this.#activate(kid(m, ".ah-dg-group-title"), true);
      this.#toggleGroup(m);
    } else if ((m = hit(e, ".ah-dg-pager-button", el))) {
      if (m instanceof HTMLButtonElement && m.disabled) { return; }
      const link = m.tagName === "A" ? m.getAttribute("href") : null;
      if (link !== null && !plainClick(e)) { return; }
      if (link !== null) { e.preventDefault(); }
      const before = this.#page;
      this.#goToPage(parseInt(m.getAttribute("data-page") || "", 10));
      if (link && this.#page !== before) { AH.apply([{ op: "url", mode: "push", value: link }]); }
    } else if ((m = hit(e, ".ah-dg-toolbar-btn[data-export]", el))) {
      void this.#export(m.getAttribute("data-export") || "");
    } else if ((m = hit(e, ".ah-dg-toolbar-btn[data-name]", el))) {
      this.#emit("ah:toolbar", { name: m.getAttribute("data-name") });
    }
  }

  // ------------------------------------------------------------------
  // Sorting, filtering, paging, grouping
  // ------------------------------------------------------------------

  #headerSort(c: Column | undefined, multi: boolean): void {
    if (!c || !c.sortable) { return; }
    let i = -1;
    this.#sort.forEach((sd, j) => { if (sd[0] === c.field) { i = j; } });
    const cur = i >= 0 ? this.#sort[i][1] : null;
    const next = cur === null ? "asc" : (cur === "asc" ? "desc" : null);
    if (multi) {
      if (i >= 0) {
        if (next) { this.#sort[i] = [c.field, next]; } else { this.#sort.splice(i, 1); }
      } else {
        this.#sort.push([c.field, "asc"]);
      }
    } else {
      this.#sort = next ? [[c.field, next]] : [];
    }
    this.#view();
    this.#emit("ah:sort", { field: c.field, value: next || "" });
  }

  #setSort(field: string, dir: string | null): void {
    this.#sort = dir ? [[field, dir]] : this.#sort.filter((sd) => sd[0] !== field);
    this.#view();
    this.#emit("ah:sort", { field, value: dir || "" });
  }

  #setFilter(field: string, v: string): void {
    if (v) { this.#filters[field] = v; } else { delete this.#filters[field]; }
    this.#page = 1;
    this.#view();
    this.#emit("ah:filter", { field, value: v });
  }

  #goToPage(p: number): void {
    const pages = Math.max(1, Math.ceil((this.#total || 0) / this.#pageSize));
    const to = Math.max(1, Math.min(pages, p));
    if (to === this.#page) { return; }
    this.#page = to;
    this.#view();
    this.#emit("ah:page", { value: to });
  }

  #toggleGroup(groupRow: Element, open?: boolean): void {
    const id = groupRow.getAttribute("data-group-id") || "";
    const collapsed = this.#collapsed.has(id);
    if (open === true && !collapsed) { return; }
    if (open === false && collapsed) { return; }
    if (collapsed) { this.#collapsed.delete(id); } else { this.#collapsed.add(id); }
    this.#viewLocal();
    this.#emit("ah:group-toggle", { value: id, expanded: collapsed ? "true" : "false" });
  }

  #setGroupBy(fields: readonly string[]): void {
    this.#groupBy = fields.filter((f) => { const c = this.#byField.get(f); return !!c && !c.check; });
    this.#collapsed = new Set();
    this.#page = 1;
    this.#view();
  }

  // ------------------------------------------------------------------
  // Column resize
  // ------------------------------------------------------------------

  #startResize(e: PointerEvent, handle: HTMLElement): void {
    const el = this.element;
    const head = handle.closest<HTMLElement>(".ah-dg-header-cell");
    const c = head ? this.#col(head.getAttribute("data-field")) : undefined;
    if (!head || !c) { return; }
    e.preventDefault();
    e.stopPropagation();
    const line = kid(el, ".ah-dg-resize-line");
    const rootLeft = el.getBoundingClientRect().left;
    const startX = e.clientX, startW = head.getBoundingClientRect().width;
    let w = startW;
    const edge = head.getBoundingClientRect().right - rootLeft;
    if (line) { setStyle(line, { display: "block", left: edge + "px", top: "0", height: el.offsetHeight + "px" }); }
    if (this.#resizeOff) { this.#resizeOff.abort(); }
    const off = this.#resizeOff = new AbortController();
    const opts = { signal: off.signal };
    document.addEventListener("pointermove", (me) => {
      w = Math.max(c.minWidth, startW + me.clientX - startX);
      if (line) { line.style.left = (edge + w - startW) + "px"; }
    }, opts);
    const end = (): void => {
      off.abort();
      this.#resizeOff = null;
      if (line) { line.style.display = "none"; }
      this.#resized = Date.now();
      w = Math.round(w);
      if (w !== c.width) {
        c.width = w;
        this.#layout();
        this.#emit("ah:column-resize", { field: c.field, value: w });
      }
    };
    document.addEventListener("pointerup", end, opts);
    document.addEventListener("pointercancel", end, opts);
  }

  // ------------------------------------------------------------------
  // Column menu
  // ------------------------------------------------------------------

  #menuItems(c: Column): object[] {
    const L = this.#labels, items: object[] = [];
    const item = (action: string, label: string, opts: { field?: string; role?: string; checked?: boolean } = {}): object =>
      ({ action, field: opts.field || c.field, label,
         role: opts.role || "menuitem", checkable: opts.checked !== undefined,
         checked: opts.checked ? "true" : "false", mark: opts.checked ? "✓" : "" });
    if (c.sortable) {
      let dir: string | null = null;
      this.#sort.forEach((sd) => { if (sd[0] === c.field) { dir = sd[1]; } });
      items.push(item("sort-asc", L.sort_asc, { role: "menuitemradio", checked: dir === "asc" }));
      items.push(item("sort-desc", L.sort_desc, { role: "menuitemradio", checked: dir === "desc" }));
      if (dir) { items.push(item("sort-clear", L.sort_clear)); }
      items.push({ sep: true });
    }
    items.push(item(c.pinned ? "unpin" : "pin", c.pinned ? L.unpin : L.pin));
    if (this.#visibleCols().length > 1) { items.push(item("hide", L.hide_column)); }
    if (!this.#remote && c.groupable) {
      items.push({ sep: true });
      const grouped = this.#groupBy.indexOf(c.field) >= 0;
      items.push(item(grouped ? "ungroup" : "group", grouped ? L.ungroup : L.group_by));
      if (this.#groupBy.length) { items.push(item("clear-groups", L.clear_groups)); }
    }
    items.push({ sep: true });
    items.push({ heading: L.columns });
    this.#dataCols().forEach((col) => {
      items.push(item("toggle-column", col.title, { field: col.field, role: "menuitemcheckbox",
                                                    checked: !col.hidden }));
    });
    return items;
  }

  #menuEntries(): HTMLElement[] { return kids(this.#menu, ".ah-dg-column-menu-item"); }

  #openMenu(field: string | null, focusIndex?: number): void {
    const c = this.#col(field);
    const menu = this.#menu;
    if (!c || c.check || !menu) { return; }
    this.#closeMenu(false);
    menu.innerHTML = AH.tpl.datagrid_column_menu({ items: this.#menuItems(c) });
    menu.setAttribute("aria-label", this.#labels.column_menu + ": " + c.title);
    menu.classList.add("ah-dg-column-menu-open");
    this.#menuField = field;
    // the header cell: its ⋮ button is only displayed while hovered
    this.#menuFloat = AH.float(menu, c.head, { placement: "bottom", align: "end", offset: 2 });
    const items = this.#menuEntries();
    const first = items[Math.min(focusIndex || 0, items.length - 1)];
    if (first) { first.focus(); }
    const off = this.#menuOff = new AbortController();
    document.addEventListener("mousedown", (e) => {
      const t = e.target as Element | null;
      if (!menu.contains(t) && !(t && t.closest && t.closest(".ah-dg-column-menu-btn"))) {
        this.#closeMenu(false);
      }
    }, { signal: off.signal });
  }

  #closeMenu(refocus: boolean): void {
    const menu = this.#menu;
    if (!menu || !menu.classList.contains("ah-dg-column-menu-open")) { return; }
    if (this.#menuOff) { this.#menuOff.abort(); this.#menuOff = null; }
    if (this.#menuFloat) { this.#menuFloat.stop(); this.#menuFloat = null; }
    menu.classList.remove("ah-dg-column-menu-open");
    setStyle(menu, { position: "", left: "", top: "" });
    menu.innerHTML = "";
    const c = this.#col(this.#menuField);
    this.#menuField = null;
    if (refocus && c) { this.#activate(c.head, true); }
  }

  #menuAction(item: HTMLElement): void {
    const action = item.getAttribute("data-action");
    const field = item.getAttribute("data-field") || "";
    const c = this.#byField.get(field);
    if (!c) { return; }
    const menuField = this.#menuField;
    switch (action) {
      case "sort-asc": this.#setSort(field, "asc"); break;
      case "sort-desc": this.#setSort(field, "desc"); break;
      case "sort-clear": this.#setSort(field, null); break;
      case "pin": case "unpin": c.pinned = action === "pin"; this.#layout(); break;
      case "hide": this.#setHidden(c, true); break;
      case "group": this.#setGroupBy(this.#groupBy.concat([field])); break;
      case "ungroup": this.#setGroupBy(this.#groupBy.filter((f) => f !== field)); break;
      case "clear-groups": this.#setGroupBy([]); break;
      case "toggle-column": {
        // stays open: several columns are usually toggled in a row
        const index = this.#menuEntries().indexOf(item);
        this.#setHidden(c, !c.hidden);
        const mc = this.#col(menuField);
        this.#openMenu(mc && mc.hidden ? this.#visibleCols()[0].field : menuField, index);
        return;
      }
    }
    this.#closeMenu(true);
  }

  #menuKey(e: KeyboardEvent): void {
    const items = this.#menuEntries();
    const i = items.indexOf(document.activeElement as HTMLElement);
    const n = items.length;
    switch (e.key) {
      case "ArrowDown": if (n) { items[(i + 1) % n].focus(); } break;
      case "ArrowUp": if (n) { items[(i - 1 + n) % n].focus(); } break;
      case "Home": if (n) { items[0].focus(); } break;
      case "End": if (n) { items[n - 1].focus(); } break;
      case "Enter": case " ": if (i >= 0) { this.#menuAction(items[i]); } break;
      case "Escape": case "Tab": this.#closeMenu(true); break;
      default: return;
    }
    e.preventDefault();
    e.stopPropagation();
  }

  // ------------------------------------------------------------------
  // Editing
  // ------------------------------------------------------------------

  #setCellValue(cell: HTMLElement, c: Column, v: string): void {
    const t = display(this.#labels, c, v);
    let span = kid(cell, ".ah-dg-cell-content");
    if (!span) {
      cell.textContent = "";
      span = document.createElement("span");
      span.className = "ah-dg-cell-content";
      cell.appendChild(span);
    }
    span.textContent = t;
    if (t !== v) { cell.setAttribute("data-v", v); } else { cell.removeAttribute("data-v"); }
  }

  #commit(cell: HTMLElement, c: Column, old: string, v: string): void {
    const row = cell.closest(".ah-dg-row");
    this.#setCellValue(cell, c, v);
    this.#emit("ah:edit", { key: row ? row.getAttribute("data-key") : null, field: c.field, value: v, old });
    if (!this.#remote) { this.#viewLocal(); }
  }

  #beginEdit(cell: HTMLElement): void {
    const c = this.#col(cell.getAttribute("data-field"));
    if (!c || !c.editable || this.#editing || !cell.classList.contains("ah-dg-cell")) { return; }
    const old = raw(cell);
    this.#activate(cell, false);
    if (c.type === "bool") {
      this.#commit(cell, c, old, old === "true" ? "false" : "true");
      this.#activate(cell, true);
      return;
    }
    let ed: HTMLInputElement | HTMLSelectElement | HTMLTextAreaElement, kind: string;
    if (c.type === "select") {
      const select = ed = document.createElement("select");
      kind = "select";
      c.options.forEach((o) => {
        select.add(new Option(String(o[1]), String(o[0]), false, String(o[0]) === old));
      });
    } else if (c.type === "textarea") {
      ed = document.createElement("textarea");
      kind = "textarea";
      ed.value = old;
    } else {
      const input = ed = document.createElement("input");
      kind = c.type === "number" ? "number" : (c.type === "date" ? "date" : "text");
      input.type = kind;
      input.value = kind === "date" ? old.slice(0, 10) : old;
    }
    ed.className = "ah-dg-editor ah-dg-editor-" + kind;
    ed.setAttribute("aria-label", c.title);
    const content = document.createDocumentFragment();
    while (cell.firstChild) { content.appendChild(cell.firstChild); }
    const off = new AbortController();
    const editing: Editing = this.#editing = { cell, col: c, old, ed, content, off };
    cell.classList.add("ah-dg-cell-editing");
    cell.appendChild(ed);
    ed.focus();
    if ((kind === "text" || kind === "textarea") && !(ed instanceof HTMLSelectElement)) { ed.select(); }
    ed.addEventListener("blur", () => {
      if (this.#editing === editing) { this.#endEdit(true, false); }
    }, { signal: off.signal });
    ed.addEventListener("keydown", (e) => { this.#editorKey(e as KeyboardEvent); }, { signal: off.signal });
  }

  #endEdit(save: boolean, refocus: boolean): void {
    const ed = this.#editing;
    if (!ed) { return; }
    this.#editing = null;
    const v = String(ed.ed.value);
    ed.off.abort();
    ed.ed.remove();
    ed.cell.appendChild(ed.content);
    ed.cell.classList.remove("ah-dg-cell-editing");
    if (save && v !== ed.old) { this.#commit(ed.cell, ed.col, ed.old, v); }
    if (refocus) { this.#activate(ed.cell, true); }
  }

  #nextEditable(cell: HTMLElement, dir: number): HTMLElement | null {
    const cols = this.#visibleCols();
    const cur = this.#col(cell.getAttribute("data-field"));
    let i = cur ? cols.indexOf(cur) : -1;
    for (i += dir; i >= 0 && i < cols.length; i += dir) {
      if (cols[i].editable) { return this.#cellAt(cell.parentElement, cols[i].field); }
    }
    return null;
  }

  #editorKey(e: KeyboardEvent): void {
    const ed = this.#editing;
    if (!ed) { return; }
    e.stopPropagation();
    if (e.key === "Enter" && (ed.col.type !== "textarea" || e.ctrlKey || e.metaKey)) {
      e.preventDefault();
      this.#endEdit(true, true);
    } else if (e.key === "Escape") {
      e.preventDefault();
      this.#endEdit(false, true);
    } else if (e.key === "Tab") {
      e.preventDefault();
      const next = this.#nextEditable(ed.cell, e.shiftKey ? -1 : 1);
      this.#endEdit(true, !next);
      if (next && next.isConnected) { this.#beginEdit(next); }
    }
  }

  // ------------------------------------------------------------------
  // Export
  // ------------------------------------------------------------------

  #exportLocal(fmt: string): Promise<void> {
    const cols = this.#dataCols().filter((c) => !c.hidden && c.type !== "command");
    const list = this.#lastView || this.#recs;
    const rows = list.map((r) => cols.map((c) => {
      const cell = cellOf(r, c.field);
      const t = text(cell);
      return t === "" ? raw(cell) : t;
    }));
    return writeFile(this.#exportName || "data", fmt, cols.map((c) => c.title), rows);
  }

  #export(fmt: string): Promise<void> {
    if (this.#remote) {
      this.#query(fmt);
      return Promise.resolve();
    }
    return this.#exportLocal(fmt);
  }
}

function withLinks(v: PagerView, t: string): PagerView {
  const url = (skip: boolean, p: number): string => skip ? "" : t.replace(/\{page\}/g, String(p));
  v.link = true;
  v.first_href = url(v.at_start, 1);
  v.prev_href = url(v.at_start, v.prev);
  v.next_href = url(v.at_end, v.next);
  v.last_href = url(v.at_end, v.last);
  v.pages.forEach((p) => { p.link = true; p.href = url(false, p.page); });
  return v;
}

AH.register("datagrid", DatagridController);
