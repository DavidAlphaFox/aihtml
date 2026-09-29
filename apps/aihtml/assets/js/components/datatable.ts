/* Controller of the data table (designs/04-components.md).
 *
 * Ported from sigil (data/datatable). The server renders every row,
 * header and pager; this file moves state around in that DOM. Local mode:
 * sort, filter row / search / advanced filters and paging over the
 * rendered rows (the pager comes from the shared template
 * datatable_pager); remote mode (data-mode "remote"): each view change
 * writes the state on the root (data-sort-field, data-sort-dir,
 * data-page, data-page-size, data-search, data-filters as JSON) and fires
 * ah:query, whose action answers with datatable_rows/3 (a morph of the
 * whole table). Selection, row details, inline editing (ah:cell-edit,
 * sent to the edit action, data-edit), column resize and the column
 * chooser work in both modes. Shared helpers are in _lib_table.ts.
 *
 * Links: with the href option the pager's prev / next / page buttons are
 * <a href> (crawlable; the page at the URL renders that state). A plain
 * left click is intercepted: the table pages as above and the link's URL
 * is pushed to the history (core's url op; back / forward reload it).
 * The pager re-rendered here fills the template (the pager's data-href)
 * from the current sort and search.
 *
 * The view state lives in the DOM (root data-* attributes, filter inputs,
 * row attributes), so a morph keeps it; the server calls refresh after
 * one. The selection is the root's data-ah-value (keys joined with
 * commas), mirrored into a hidden input; "change" fires when the user
 * changes it, never from methods.
 *
 * Events (native CustomEvents, bubbling; the detail in e.detail):
 *   ah:query          {sort, dir, page, pageSize, search, filters} (QueryEvent)
 *   ah:sort           {field, dir}                (SortEvent)
 *   ah:page           {page, pageSize}            (PageEvent)
 *   ah:filter         {filters, search}           (FilterEvent)
 *   ah:row-click, ah:row-dblclick, ah:row-expand, ah:row-collapse  {key}
 *                     (RowEvent; the key is also the root's data-key)
 *   ah:cell-edit      {key, field, value, old}, fired on the cell (CellEditEvent)
 *   ah:columns        {hidden}                    (ColumnsEvent)
 *   ah:column-resize  {field, width}              (ColumnResizeEvent)
 */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import {
  OWN, TableFrame, clickSelect, compare, focusRow, headerCheck, isNum, isOff, keySelect, keysOf, kid,
  kids, markRows, nextSort, ord, part, partTable, raw, rowKey, sameKeys, toDir, toKeys, writeSort,
  writeValue
} from "./_lib_table.ts";
import type { SortEvent } from "./_lib_table.ts";
import { join, split } from "./_lib_values.ts";
import "virtual:ah-tpl/datatable_pager";

export type { SortEvent, ColumnResizeEvent } from "./_lib_table.ts";

/** A column filter: the filter row's text, or an advanced condition. */
export type ColumnFilter = string | { condition: string; value: string };
export type Filters = Record<string, ColumnFilter>;

/** Detail of ah:query. */
export interface QueryEvent {
  sort: string | null; dir: string | null; page: number; pageSize: number | null;
  search: string; filters: Filters;
}
/** Detail of ah:page. */
export interface PageEvent { page: number; pageSize: number; }
/** Detail of ah:filter. */
export interface FilterEvent { filters: Filters; search: string; }
/** Detail of ah:row-click, ah:row-dblclick, ah:row-expand, ah:row-collapse. */
export interface RowEvent { key: string; }
/** Detail of ah:cell-edit. */
export interface CellEditEvent { key: string; field: string | null; value: string; old: string; }
/** Detail of ah:columns. */
export interface ColumnsEvent { hidden: string[]; }

/** The pager's labels (data-* of the pager container). */
interface PagerTexts { info: string | null; prev: string | null; next: string | null; size: string | null; }

/** The open cell editor. */
interface Edit {
  td: HTMLTableCellElement;
  tr: HTMLTableRowElement;
  field: string | null;
  type: string;
  old: string;
  html: string;
  input: HTMLInputElement;
}

const FILTER_INPUTS = ".ah-dt-filter-input, .ah-dt-adv-filter-input, .ah-dt-search-input";
const ROW = "tbody > tr.ah-dt-row";

function qa<E extends Element = HTMLElement>(root: ParentNode | null, sel: string): E[] {
  return root ? Array.from(root.querySelectorAll<E>(sel)) : [];
}
function num(v: unknown, d: number): number { const n = parseInt(String(v), 10); return n > 0 ? n : d; }
function lower(s: unknown): string { return String(s).toLowerCase(); }
function cellOf(tr: Element, field: string | null): HTMLTableCellElement | null {
  return kids<HTMLTableCellElement>(tr, "td[data-field]").filter((td) => td.getAttribute("data-field") === field)[0] || null;
}
function siblingInput(sel: Element): HTMLInputElement | null {
  return sel.parentElement ? kid<HTMLInputElement>(sel.parentElement, ".ah-dt-adv-filter-input") : null;
}
function expandBtns(tr: Element): HTMLElement[] {
  const out: HTMLElement[] = [];
  kids(tr, "td").forEach((td) => { out.push(...kids(td, ".ah-dt-expand-btn")); });
  return out;
}

function matchCond(cond: string, v: string, f: string, number: boolean): boolean {
  const t = lower(v), q = lower(f);
  const nv = parseFloat(v), nf = parseFloat(f), both = isNum(v) && isNum(f);
  switch (cond) {
    case "empty": return v === "";
    case "not_empty": return v !== "";
    case "contains": return t.indexOf(q) >= 0;
    case "not_contains": return t.indexOf(q) < 0;
    case "starts_with": return t.indexOf(q) === 0;
    case "ends_with": return t.length >= q.length && t.slice(t.length - q.length) === q;
    case "equals": return number ? both && nv === nf : t === q;
    case "not_equals": return number ? !(both && nv === nf) : t !== q;
    case "gt": return both && nv > nf;
    case "gte": return both && nv >= nf;
    case "lt": return both && nv < nf;
    case "lte": return both && nv <= nf;
    default: return true;
  }
}

function rowMatches(tr: Element, fields: readonly (string | null)[], search: string, filters: Filters,
                    types: Record<string, string | null>): boolean {
  if (search) {
    const hit = fields.some((f) => lower(raw(cellOf(tr, f))).indexOf(search) >= 0);
    if (!hit) { return false; }
  }
  return Object.keys(filters).every((f) => {
    const flt = filters[f], v = raw(cellOf(tr, f));
    if (typeof flt === "string") { return lower(v).indexOf(lower(flt)) >= 0; }
    return matchCond(flt.condition, v, flt.value, types[f] === "number");
  });
}

/** The pager's view data, as aihtml_datatable:pager_view/6 builds it;
 *  base is the href template with {sort} and {search} filled in, or null. */
function pagerView(page: number, size: number, total: number, sizes: readonly number[], t: PagerTexts,
                   base: string | null): object {
  const pages = Math.max(1, Math.ceil(total / size));
  const start = total === 0 ? 0 : (page - 1) * size + 1;
  const end = Math.min(page * size, total);
  const info = (t.info || "")
    .split("{start}").join(String(start)).split("{end}").join(String(end)).split("{total}").join(String(total))
    .split("{page}").join(String(page)).split("{pages}").join(String(pages));
  let btns: number[];
  if (pages <= 7) {
    btns = [];
    for (let i = 1; i <= pages; i++) { btns.push(i); }
  } else {
    btns = [1];
    if (page > 3) { btns.push(0); }
    for (let m = Math.max(2, page - 1); m <= Math.min(page + 1, pages - 1); m++) { btns.push(m); }
    if (page < pages - 2) { btns.push(0); }
    btns.push(pages);
  }
  const url = (p: number): string =>
    base != null ? base.split("{page}").join(String(p)).split("{size}").join(String(size)) : "";
  const link = base != null;
  const all = sizes.concat([size]).filter((s, j, a) => a.indexOf(s) === j).sort((a, b) => a - b);
  return {
    info, prev_label: t.prev, next_label: t.next, size_label: t.size,
    prev_disabled: page <= 1, next_disabled: page >= pages,
    prev_link: link && page > 1, prev_href: url(page - 1),
    next_link: link && page < pages, next_href: url(page + 1),
    buttons: btns.map((b) => b === 0 ? { gap: true, page: 0, active: false, link: false, href: "" }
      : { gap: false, page: b, active: b === page, link: link && b !== page, href: url(b) }),
    has_sizes: sizes.length > 0,
    sizes: all.map((s) => ({ size: s, selected: s === size }))
  };
}

// A plain left click on a pager link is handled in place; any other
// click (new tab, window, download) is left to the browser.
function plainClick(e: MouseEvent): boolean {
  return e.button === 0 && !e.metaKey && !e.ctrlKey && !e.shiftKey && !e.altKey;
}

function pushUrl(url: string): void { AH.apply([{ op: "url", mode: "push", value: url }]); }

class DatatableController extends AH.Controller {
  #anchor: string | null = null;
  #edit: Edit | null = null;
  #float: FloatHandle | null = null;
  #timer: ReturnType<typeof setTimeout> | undefined = undefined;
  #chooserOff: AbortController | null = null;
  readonly #frame = new TableFrame(this, "ah-dt");

  override setup(): void {
    const el = this.element;
    this.#anchor = null;
    this.#edit = null;
    const debounced = (): void => {
      clearTimeout(this.#timer);
      this.#timer = setTimeout(() => { this.#filtered(); }, 200);
    };
    this.delegate<MouseEvent, HTMLTableRowElement>("click", ROW, (e, tr) => { this.#click(e, tr); });
    this.delegate<MouseEvent, HTMLTableRowElement>("dblclick", ROW, (e, tr) => {
      if (tr.parentNode !== this.#body) { return; }
      const td = (e.target as Element).closest<HTMLTableCellElement>("td.ah-dt-cell-editable");
      if (td && td.parentNode === tr) { this.#beginEdit(td); }
      this.#event("ah:row-dblclick", rowKey(tr));
    });
    this.listen(el, "keydown", (e) => {
      if (!(e.target instanceof Element)) { return; }
      if (e.target.matches(".ah-dt-editor")) { this.#editKey(e); return; }
      const tr = e.target.closest<HTMLTableRowElement>(ROW);
      if (tr && el.contains(tr)) { this.#keydown(e, tr); return; }
      const th = e.target.closest<HTMLElement>(".ah-dt-th-sortable");
      if (th && el.contains(th) && (e.key === "Enter" || e.key === " ")) {
        e.preventDefault();
        th.click();
      }
    });
    this.delegate("focusout", ".ah-dt-editor", () => {
      const ed = this.#edit;
      setTimeout(() => { if (this.#edit === ed && ed) { this.#endEdit(true); } }, 0);
    });
    // Inner controls (row / header boxes, filters, pager, chooser) report
    // to the table, not as its own change: stopImmediatePropagation also
    // keeps them from listeners on the root added after this one, as
    // jQuery's delegated stopPropagation did.
    this.listen(el, "change", (e) => {
      const t = e.target;
      if (!(t instanceof Element) || t === el) { return; }
      if (t.matches(".ah-dt-row-checkbox")) {
        e.stopImmediatePropagation();
        // a row box sits in its row (server markup)
        const k = rowKey(t.closest("tr") as HTMLTableRowElement);
        const rest = keysOf(el).filter((x) => x !== k);
        this.#select((t as HTMLInputElement).checked ? rest.concat([k]) : rest, true);
      } else if (t.matches(".ah-dt-header-checkbox")) {
        e.stopImmediatePropagation();
        const page = this.#shown().map(rowKey);
        const others = keysOf(el).filter((x) => page.indexOf(x) < 0);
        this.#select((t as HTMLInputElement).checked ? others.concat(page) : others, true);
      } else if (t.matches(FILTER_INPUTS)) {
        e.stopImmediatePropagation();
      } else if (t.matches(".ah-dt-adv-filter-select")) {
        e.stopImmediatePropagation();
        const v = (t as HTMLSelectElement).value;
        const none = v === "empty" || v === "not_empty";
        const input = siblingInput(t);
        if (input) {
          input.disabled = none;
          if (none) { input.value = ""; }
        }
        this.#filtered();
      } else if (t.matches(".ah-dt-pager-size-select")) {
        e.stopImmediatePropagation();
        el.setAttribute("data-page-size", String(num((t as HTMLSelectElement).value, 10)));
        this.#goTo(1);
      } else if (t.matches(".ah-dt-chooser-checkbox")) {
        e.stopImmediatePropagation();
        this.#setHidden(t.getAttribute("data-field") || "", !(t as HTMLInputElement).checked);
      }
    });
    this.delegate("click", ".ah-dt-th-sortable", (e, th) => {
      if ((e.target as Element).closest(".ah-dt-resize-handle") || isOff(el)) { return; }
      const field = th.getAttribute("data-field");
      const dir = nextSort(el, field);
      writeSort(el, "ah-dt", dir ? field : null, dir);
      el.setAttribute("data-page", "1");
      this.#view();
      this.fire<SortEvent>("ah:sort", { field, dir });
    });
    this.delegate("input", FILTER_INPUTS, (e) => {
      e.stopImmediatePropagation();
      debounced();
    });
    // Pager buttons; with href they are links: a plain click pages in
    // place and pushes the link's URL, other clicks go to the browser.
    const pagerClick = (e: MouseEvent, b: HTMLElement, page: number): void => {
      const href = b.tagName === "A" ? b.getAttribute("href") : null;
      if (href !== null) {
        if (!plainClick(e)) { return; }
        e.preventDefault();
      }
      this.#goTo(page);
      if (href !== null) { pushUrl(href); }
    };
    this.delegate("click", ".ah-dt-pager-btn-num", (e, b) => { pagerClick(e, b, num(b.getAttribute("data-page"), 1)); });
    this.delegate("click", ".ah-dt-pager-btn-prev", (e, b) => { pagerClick(e, b, this.#page - 1); });
    this.delegate("click", ".ah-dt-pager-btn-next", (e, b) => { pagerClick(e, b, this.#page + 1); });
    this.delegate("click", ".ah-dt-chooser-btn", () => {
      const p = kid(el, ".ah-dt-chooser-panel");
      this.#chooser(!(p && p.classList.contains("ah-dt-chooser-panel-open")));
    });
    this.listen(el, "ah:error", () => { this.#loading(false); });
    this.#frame.bind();
    if (this.#remote) { this.#stripes(this.#shown()); } else { this.#apply(); }
  }

  override teardown(): void {
    clearTimeout(this.#timer);
    if (this.#float) { this.#float.stop(); this.#float = null; }
    this.#frame.stop();
    if (this.#chooserOff) { this.#chooserOff.abort(); this.#chooserOff = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v: unknown): void {
    this.#select(toKeys(v).filter((k) => this.#byKey(k)), false);
  }
  clearSelection(): void { this.#select([], false); }
  sort(field: unknown, dir?: unknown): void {
    const el = this.element;
    const d = toDir(dir);
    writeSort(el, "ah-dt", d ? String(field) : null, d);
    el.setAttribute("data-page", "1");
    this.#view();
  }
  goToPage(page: unknown): void { this.element.setAttribute("data-page", String(num(page, 1))); this.#view(); }
  setPageSize(size: unknown): void {
    const el = this.element;
    el.setAttribute("data-page-size", String(num(size, 10)));
    el.setAttribute("data-page", "1");
    this.#view();
  }
  setSearch(text: unknown): void {
    const el = this.element;
    const s = text ? String(text) : "";
    qa<HTMLInputElement>(this.#header, ".ah-dt-search-input").forEach((i) => { i.value = s; });
    if (s) { el.setAttribute("data-search", s); } else { el.removeAttribute("data-search"); }
    el.setAttribute("data-page", "1");
    this.#view();
  }
  clearFilters(): void {
    const el = this.element, h = this.#header;
    qa<HTMLInputElement>(h, FILTER_INPUTS).forEach((i) => { i.value = ""; });
    qa<HTMLSelectElement>(h, ".ah-dt-adv-filter-select").forEach((s) => { s.value = "contains"; });
    qa<HTMLInputElement>(h, ".ah-dt-adv-filter-input").forEach((i) => { i.disabled = false; });
    el.removeAttribute("data-search");
    el.setAttribute("data-page", "1");
    this.#view();
  }
  showColumn(field: unknown): void { this.#setHidden(String(field), false); }
  hideColumn(field: unknown): void { this.#setHidden(String(field), true); }
  expandRow(key: unknown): void { this.#setDetails(String(key), true, false); }
  collapseRow(key: unknown): void { this.#setDetails(String(key), false, false); }
  refresh(): void {
    const el = this.element;
    markRows(this.#rows, "ah-dt", keysOf(el), el.getAttribute("data-selection"));
    split(el.getAttribute("data-hidden")).filter(Boolean).forEach((f) => {
      this.#rows.forEach((r) => { const c = cellOf(r, f); if (c) { c.hidden = true; } });
    });
    split(el.getAttribute("data-expanded")).filter(Boolean).forEach((k) => {
      const d = this.#detail(k);
      if (d && d.classList.contains("ah-dt-row-details-hidden")) {
        d.classList.remove("ah-dt-row-details-hidden");
        const tr = this.#byKey(k);
        if (tr) {
          expandBtns(tr).forEach((b) => {
            b.classList.add("ah-dt-expand-btn-open");
            b.setAttribute("aria-expanded", "true");
          });
        }
      }
    });
    this.#loading(false);
    if (this.#remote) { this.#stripes(this.#shown()); } else { this.#apply(); }
  }

  // ---- the DOM ------------------------------------------------------

  get #header(): HTMLElement | null { return part(this.element, "ah-dt", "header"); }
  get #body(): HTMLTableSectionElement | null {
    return document.getElementById(this.element.id + "-rows") as HTMLTableSectionElement | null;
  }
  get #rows(): HTMLTableRowElement[] { return kids<HTMLTableRowElement>(this.#body, "tr.ah-dt-row"); }
  get #remote(): boolean { return this.element.getAttribute("data-mode") === "remote"; }
  get #page(): number { return num(this.element.getAttribute("data-page"), 1); }
  get #editable(): boolean { return this.element.getAttribute("data-editable") === "true"; }
  #shown(): HTMLTableRowElement[] { return this.#rows.filter((r) => !r.hidden); }
  #byKey(key: unknown): HTMLTableRowElement | null {
    const k = String(key);
    return this.#rows.filter((r) => r.getAttribute("data-key") === k)[0] || null;
  }
  #detail(key: string): HTMLTableRowElement | null {
    return kids<HTMLTableRowElement>(this.#body, "tr.ah-dt-row-details")
      .filter((r) => r.getAttribute("data-key") === key)[0] || null;
  }
  #headerRow(): HTMLTableRowElement | null {
    return kid<HTMLTableRowElement>(kid(partTable(this.element, "ah-dt", "header"), "thead"), "tr.ah-dt-header-row");
  }
  #th(field: string | null): HTMLTableCellElement | null {
    return kids<HTMLTableCellElement>(this.#headerRow(), "th[data-field]")
      .filter((th) => th.getAttribute("data-field") === field)[0] || null;
  }
  #headerBox(): HTMLInputElement | null {
    return qa<HTMLInputElement>(this.#header, ".ah-dt-header-checkbox")[0] || null;
  }
  #loading(on: boolean): void {
    kids(this.element, ".ah-dt-content").forEach((c) => { c.classList.toggle("ah-dt-loading", on); });
  }

  // Column filters from the filter row: text, or {condition, value}.
  #filters(): Filters {
    const f: Filters = {};
    const h = this.#header;
    qa<HTMLInputElement>(h, ".ah-dt-filter-input").forEach((i) => {
      if (i.value) { f[i.getAttribute("data-field") || ""] = i.value; }
    });
    qa<HTMLSelectElement>(h, ".ah-dt-adv-filter-select").forEach((s) => {
      const cond = s.value;
      const input = siblingInput(s);
      const v = (input && input.value) || "";
      if (cond === "empty" || cond === "not_empty") { f[s.getAttribute("data-field") || ""] = { condition: cond, value: "" }; }
      else if (v) { f[s.getAttribute("data-field") || ""] = { condition: cond, value: v }; }
    });
    return f;
  }

  #search(): string {
    const s = qa<HTMLInputElement>(this.#header, ".ah-dt-search-input")[0];
    return s ? s.value : (this.element.getAttribute("data-search") || "");
  }

  // ---- the view ----------------------------------------------------

  #pager(page: number, size: number, total: number): void {
    const c = kid(this.element, ".ah-dt-pager-container");
    if (!c || !AH.tpl || !AH.tpl.datatable_pager) { return; }
    const sizes = (c.getAttribute("data-sizes") || "").split(",").filter(Boolean).map(Number);
    const t: PagerTexts = { info: c.getAttribute("data-info"), prev: c.getAttribute("data-prev"),
                            next: c.getAttribute("data-next"), size: c.getAttribute("data-size") };
    c.innerHTML = AH.tpl.datatable_pager(pagerView(page, size, total, sizes, t, this.#linkBase(c)));
  }

  // The href template (data-href of the pager) with {sort} and {search}
  // filled in from the current view, as aihtml_datatable:link_base/3 does.
  #linkBase(c: Element): string | null {
    const tpl = c.getAttribute("data-href");
    if (tpl == null) { return null; }
    const el = this.element;
    const field = el.getAttribute("data-sort-field"), dir = el.getAttribute("data-sort-dir");
    const sort = field && dir ? field + ":" + dir : "";
    return tpl.split("{sort}").join(encodeURIComponent(sort))
      .split("{search}").join(encodeURIComponent(this.#search()));
  }

  #stripes(shown: readonly HTMLTableRowElement[]): void {
    const el = this.element;
    const alt = el.getAttribute("data-alt-rows") === "true";
    shown.forEach((r, i) => { r.classList.toggle("ah-dt-row-alt", alt && i % 2 === 1); });
    kids(this.#body, "tr.ah-dt-row-empty").forEach((r) => { r.hidden = shown.length > 0; });
    const rows = this.#rows;
    const stop = rows.filter((r) => r.getAttribute("tabindex") === "0")[0];
    if (!stop || stop.hidden) { focusRow(rows, shown[0], false); }
    headerCheck(this.#headerBox(), keysOf(el), shown.map(rowKey));
  }

  // Local mode: search, filters, sort and page over the rendered rows.
  #apply(): void {
    const el = this.element;
    const body = this.#body;
    const rows = this.#rows;
    const fields: (string | null)[] = [], types: Record<string, string | null> = {};
    kids(this.#headerRow(), "th[data-field]").forEach((th) => {
      fields.push(th.getAttribute("data-field"));
      types[th.getAttribute("data-field") || ""] = th.getAttribute("data-type");
    });
    const search = lower(String(this.#search()).trim());
    const filters = this.#filters();
    const field = el.getAttribute("data-sort-field"), dir = el.getAttribute("data-sort-dir");
    const sign = dir === "desc" ? -1 : 1;
    const match = rows.filter((r) => rowMatches(r, fields, search, filters, types));
    match.sort((a, b) => {
      const c = field && dir ? sign * compare(raw(cellOf(a, field)), raw(cellOf(b, field))) : 0;
      return c || ord(a) - ord(b);
    });
    const rest = rows.filter((r) => match.indexOf(r) < 0).sort((a, b) => ord(a) - ord(b));
    if (body) {
      const end = kid(body, "tr.ah-dt-row-empty");
      match.concat(rest).forEach((r) => {
        body.insertBefore(r, end);
        const d = this.#detail(rowKey(r));
        if (d) { body.insertBefore(d, end); }
      });
    }
    const size = num(el.getAttribute("data-page-size"), 0);
    const total = match.length;
    let page = this.#page;
    if (size) {
      page = Math.min(page, Math.max(1, Math.ceil(total / size)));
      el.setAttribute("data-page", String(page));
    }
    const shown = size ? match.slice((page - 1) * size, page * size) : match;
    rows.forEach((r) => {
      const on = shown.indexOf(r) >= 0;
      r.hidden = !on;
      const d = this.#detail(rowKey(r));
      if (d) { d.hidden = !on; }
    });
    this.#stripes(shown);
    if (size) { this.#pager(page, size, total); }
  }

  // Remote mode: the state goes on the root and the server answers.
  #query(): void {
    const el = this.element;
    const f = this.#filters();
    if (Object.keys(f).length) { el.setAttribute("data-filters", JSON.stringify(f)); }
    else { el.removeAttribute("data-filters"); }
    const s = this.#search();
    if (s) { el.setAttribute("data-search", s); } else { el.removeAttribute("data-search"); }
    this.#loading(true);
    this.fire<QueryEvent>("ah:query", {
      sort: el.getAttribute("data-sort-field"), dir: el.getAttribute("data-sort-dir"),
      page: this.#page,
      pageSize: num(el.getAttribute("data-page-size"), 0) || null,
      search: s, filters: f
    });
  }

  #view(): void {
    if (this.#remote) { this.#query(); } else { this.#apply(); }
  }

  #goTo(page: number): void {
    const el = this.element;
    el.setAttribute("data-page", String(Math.max(1, page)));
    this.#view();
    this.fire<PageEvent>("ah:page", { page: this.#page, pageSize: num(el.getAttribute("data-page-size"), 0) });
  }

  #filtered(): void {
    this.element.setAttribute("data-page", "1");
    this.#view();
    this.fire<FilterEvent>("ah:filter", { filters: this.#filters(), search: this.#search() });
  }

  // ---- selection, details, columns ------------------------------------

  #select(keys: string[], user: boolean): void {
    const el = this.element;
    const prev = keysOf(el);
    markRows(this.#rows, "ah-dt", keys, el.getAttribute("data-selection"));
    writeValue(el, keys);
    headerCheck(this.#headerBox(), keys, this.#shown().map(rowKey));
    if (user && !sameKeys(prev, keys)) { this.fire("change"); }
  }

  #event(name: string, key: string): void {
    this.element.setAttribute("data-key", key);
    this.fire<RowEvent>(name, { key });
  }

  #setDetails(key: string, open: boolean, user: boolean): void {
    const el = this.element;
    const tr = this.#byKey(key), d = this.#detail(key);
    if (!tr || !d) { return; }
    const was = !d.classList.contains("ah-dt-row-details-hidden");
    if (was === open) { return; }
    d.classList.toggle("ah-dt-row-details-hidden", !open);
    expandBtns(tr).forEach((b) => {
      b.classList.toggle("ah-dt-expand-btn-open", open);
      b.setAttribute("aria-expanded", String(open));
    });
    const list = split(el.getAttribute("data-expanded")).filter((k) => k && k !== key);
    if (open) { list.push(key); }
    if (list.length) { el.setAttribute("data-expanded", join(list)); } else { el.removeAttribute("data-expanded"); }
    if (user) { this.#event(open ? "ah:row-expand" : "ah:row-collapse", key); }
  }

  #setHidden(field: string, hide: boolean): void {
    const el = this.element;
    const th = this.#th(field);
    if (!th) { return; }
    const row = th.parentElement as HTMLTableRowElement;   // the header row
    const idx = Array.prototype.indexOf.call(row.children, th);
    [partTable(el, "ah-dt", "header"), partTable(el, "ah-dt", "body")].forEach((t) => {
      kids(t, "colgroup").forEach((g) => {
        const c = g.children[idx];
        if (c instanceof HTMLElement) { c.hidden = hide; }
      });
    });
    th.hidden = hide;
    kids(kid(partTable(el, "ah-dt", "header"), "thead"), "tr.ah-dt-filter-row").forEach((tr) => {
      const c = tr.children[idx];
      if (c instanceof HTMLElement) { c.hidden = hide; }
    });
    this.#rows.forEach((r) => { const c = cellOf(r, field); if (c) { c.hidden = hide; } });
    const hidden = kids(row, "th[data-field]").filter((h) => h.hidden)
      .map((h) => h.getAttribute("data-field") || "");
    if (hidden.length) { el.setAttribute("data-hidden", join(hidden)); } else { el.removeAttribute("data-hidden"); }
    const span = kids(row, "th").filter((h) => !h.hidden).length;
    kids(this.#body, "tr").forEach((tr) => {
      kids(tr, "td.ah-dt-cell-empty, td.ah-dt-row-details-cell").forEach((td) => {
        td.setAttribute("colspan", String(span));
      });
    });
    kids(el, ".ah-dt-chooser-panel").forEach((p) => {
      qa<HTMLInputElement>(p, ".ah-dt-chooser-checkbox").forEach((cb) => {
        if (cb.getAttribute("data-field") === field) { cb.checked = !hide; }
      });
    });
    this.fire<ColumnsEvent>("ah:columns", { hidden });
  }

  // ---- inline editing ------------------------------------------------

  #beginEdit(td: HTMLTableCellElement | null | undefined): void {
    if (!td || !this.#editable || isOff(this.element)) { return; }
    if (this.#edit) { this.#endEdit(true); }
    const tr = td.parentElement as HTMLTableRowElement;     // a cell's row
    const field = td.getAttribute("data-field");
    const th = this.#th(field);
    const type = (th && th.getAttribute("data-type")) || "text";
    const old = raw(td);
    const input = document.createElement("input");
    input.className = "ah-dt-editor ah-dt-editor-" + type;
    if (type === "checkbox") {
      input.type = "checkbox";
      input.checked = old === "true";
    } else {
      input.type = type === "number" ? "number" : (type === "date" ? "date" : "text");
      input.value = old;
    }
    this.#edit = { td, tr, field, type, old, html: td.innerHTML, input };
    td.innerHTML = "";
    td.appendChild(input);
    input.focus();
    if (type !== "checkbox" && input.select) { input.select(); }
  }

  // Commit (or cancel) the open editor; returns the edited cell.
  #endEdit(commit: boolean): HTMLTableCellElement | null {
    const ed = this.#edit;
    if (!ed) { return null; }
    const el = this.element;
    this.#edit = null;
    const value = ed.type === "checkbox" ? String(ed.input.checked) : ed.input.value;
    if (!commit || value === ed.old) {
      ed.td.innerHTML = ed.html;
      return ed.td;
    }
    ed.td.innerHTML = "";
    const span = document.createElement("span");
    span.textContent = value;
    ed.td.appendChild(span);
    ed.td.setAttribute("data-value", value);
    const key = rowKey(ed.tr);
    ed.td.setAttribute("data-key", key);
    ed.td.setAttribute("data-old", ed.old);
    ed.td.setAttribute("data-table", el.id);
    const token = el.getAttribute("data-edit");
    if (token && !ed.td.hasAttribute("data-ah-on")) {
      ed.td.setAttribute("data-ah-on", "ah:cell-edit:" + token);
      AH.mount(ed.td);
    }
    this.fire<CellEditEvent>("ah:cell-edit", { key, field: ed.field, value, old: ed.old }, ed.td);
    return ed.td;
  }

  #editKey(e: KeyboardEvent): void {
    const ed = this.#edit;
    if (!ed || e.target !== ed.input) { return; }
    e.stopPropagation();
    if (e.key === "Escape") {
      e.preventDefault();
      this.#endEdit(false);
      focusRow(this.#rows, ed.tr, true);
    } else if (e.key === "Enter") {
      e.preventDefault();
      this.#endEdit(true);
      focusRow(this.#rows, ed.tr, true);
    } else if (e.key === "Tab") {
      e.preventDefault();
      const cells: HTMLTableCellElement[] = [];
      this.#shown().forEach((r) => {
        kids<HTMLTableCellElement>(r, "td.ah-dt-cell-editable").forEach((td) => { if (!td.hidden) { cells.push(td); } });
      });
      const i = cells.indexOf(ed.td);
      const next = cells[i + (e.shiftKey ? -1 : 1)];
      this.#endEdit(true);
      if (next) { this.#beginEdit(next); } else { focusRow(this.#rows, ed.tr, true); }
    }
  }

  // ---- column chooser -------------------------------------------------

  #chooser(open: boolean): void {
    const el = this.element;
    const p = kid(el, ".ah-dt-chooser-panel");
    const btn = qa(this.#header, ".ah-dt-chooser-btn")[0];
    if (!p || !btn) { return; }
    if (this.#float) { this.#float.stop(); this.#float = null; }
    if (this.#chooserOff) { this.#chooserOff.abort(); this.#chooserOff = null; }
    p.classList.toggle("ah-dt-chooser-panel-open", open);
    btn.setAttribute("aria-expanded", String(open));
    if (!open) { return; }
    this.#float = AH.float(p, btn, { placement: "bottom", align: "end", offset: 4 });
    const off = this.#chooserOff = new AbortController();
    document.addEventListener("mousedown", (e) => {
      const t = e.target as Node | null;
      if (!p.contains(t) && !btn.contains(t)) { this.#chooser(false); }
    }, { signal: off.signal });
    document.addEventListener("keydown", (e) => {
      if (e.key === "Escape") { this.#chooser(false); btn.focus(); }
    }, { signal: off.signal });
  }

  // ---- events -----------------------------------------------------------

  #keydown(e: KeyboardEvent, tr: HTMLTableRowElement): void {
    const el = this.element;
    if (e.target !== tr || tr.parentNode !== this.#body || isOff(el) || e.altKey || e.metaKey) { return; }
    const vis = this.#shown();
    const i = vis.indexOf(tr);
    const key = rowKey(tr);
    let to: HTMLTableRowElement | null | undefined = null;
    let select = false;
    switch (e.key) {
      case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
      case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
      case "Home": to = vis[0]; break;
      case "End": to = vis[vis.length - 1]; break;
      case "PageDown": to = vis[Math.min(i + 10, vis.length - 1)]; break;
      case "PageUp": to = vis[Math.max(i - 10, 0)]; break;
      case "ArrowRight": this.#setDetails(key, true, true); break;
      case "ArrowLeft": this.#setDetails(key, false, true); break;
      case "F2":
      case "Enter": {
        const cell = this.#editable
          ? kids<HTMLTableCellElement>(tr, "td.ah-dt-cell-editable").filter((td) => !td.hidden)[0] : undefined;
        if (cell) { this.#beginEdit(cell); }
        else if (e.key === "Enter") { select = true; }
        break;
      }
      case " ": select = true; break;
      default:
        return;
    }
    if (select) {
      const next = keySelect(el.getAttribute("data-selection"), keysOf(el), key);
      if (next) { this.#select(next, true); this.#anchor = key; }
    }
    e.preventDefault();
    if (to) { focusRow(this.#rows, to, true); }
  }

  #click(e: MouseEvent, tr: HTMLTableRowElement): void {
    const el = this.element;
    if (tr.parentNode !== this.#body || isOff(el)) { return; }
    const key = rowKey(tr);
    const t = e.target as Element;
    const btn = t.closest(".ah-dt-expand-btn");
    if (btn && tr.contains(btn)) {
      this.#setDetails(key, !btn.classList.contains("ah-dt-expand-btn-open"), true);
      return;
    }
    if (this.#edit && this.#edit.td.contains(t) && this.#edit.td !== t) { return; }
    focusRow(this.#rows, tr, false);
    const own = t.closest(OWN);
    if (own && tr.contains(own)) { return; }
    const next = clickSelect(el.getAttribute("data-selection"), keysOf(el), key, e, this.#anchor,
                             this.#shown().map(rowKey));
    if (next) {
      this.#select(next, true);
      if (!e.shiftKey) { this.#anchor = key; }
    }
    this.#event("ah:row-click", key);
  }
}

AH.register("datatable", DatatableController);
