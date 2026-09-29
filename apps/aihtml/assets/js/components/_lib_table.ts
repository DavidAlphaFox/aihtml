/* Shared by the treegrid and datatable controllers (treegrid.ts,
 * datatable.ts): selection by row keys, sorting order, roving focus, and
 * (TableFrame) the header / body scroll and gutter sync and column
 * resizing. Ported from sigil (data/treegrid, data/datatable). Native DOM:
 * every helper takes elements (arrays of rows), never jQuery objects; the
 * listeners TableFrame binds go through the AH.Controller, so teardown
 * removes them.
 */
import type { Controller } from "../core.ts";
import { join, split } from "./_lib_values.ts";

/** The row selection mode (data-selection). */
export type SelectionMode = "none" | "single" | "multiple" | "checkbox" | string;

/** A sort direction; null: unsorted. */
export type SortDir = "asc" | "desc" | null;

/** Detail of ah:sort. */
export interface SortEvent { field: string | null; dir: SortDir; }

/** Detail of ah:column-resize. */
export interface ColumnResizeEvent { field: string | null; width: number; }

/** The element children of el matching selector. */
export function kids<E extends Element = HTMLElement>(el: Element | null | undefined, selector: string): E[] {
  if (!el) { return []; }
  return Array.prototype.filter.call(el.children, (c: Element) => c.matches(selector)) as E[];
}

export function kid<E extends Element = HTMLElement>(el: Element | null | undefined, selector: string): E | null {
  return kids<E>(el, selector)[0] || null;
}

/** The key of a row (data-key, which the server writes on every row). */
export function rowKey(tr: Element): string { return tr.getAttribute("data-key") ?? ""; }

/** The selected keys, joined by _lib_values (a comma in a key is escaped). */
export function keysOf(el: Element): string[] {
  return split(el.getAttribute("data-ah-value") || "");
}

export function toKeys(v: unknown): string[] {
  if (v === null || v === undefined) { return []; }
  return split(Array.isArray(v) ? v : String(v));
}

export function writeValue(el: Element, keys: readonly string[]): void {
  const v = join(keys);
  el.setAttribute("data-ah-value", v);
  kids<HTMLInputElement>(el, "input[type=hidden][data-ah-input]").forEach((i) => { i.value = v; });
}

export function sameKeys(a: readonly string[], b: readonly string[]): boolean {
  return a.length === b.length && a.every((k, i) => k === b[i]);
}

/** The raw value of a cell: data-value when the server wrote one
 *  (numbers, rendered cells), its text otherwise. */
export function raw(td: Element | null | undefined): string {
  if (!td) { return ""; }
  const v = td.getAttribute("data-value");
  return v !== null ? v : (td.textContent || "");
}

export function isNum(s: string): boolean { return /^\s*-?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?\s*$/.test(s); }

/** Numbers before texts, texts in the locale's order. */
export function compare(a: string, b: string): number {
  const na = isNum(a), nb = isNum(b);
  if (na && nb) { return parseFloat(a) - parseFloat(b); }
  if (na !== nb) { return na ? -1 : 1; }
  return a.localeCompare(b, undefined, { numeric: true, sensitivity: "base" });
}

/** The original position of a row (data-i). */
export function ord(tr: Element): number { return parseInt(tr.getAttribute("data-i") || "", 10) || 0; }

/** Selection after a click on a row; null: the click does not select. */
export function clickSelect(mode: SelectionMode | null, keys: string[], key: string, e: MouseEvent,
                            anchor: string | null, order: readonly string[]): string[] | null {
  if (mode === "single") {
    if (keys.length === 1 && keys[0] === key) { return e.detail > 1 ? keys : []; }
    return [key];
  }
  if (mode === "multiple") {
    if (e.shiftKey && anchor !== null) {
      const a = order.indexOf(anchor), b = order.indexOf(key);
      if (a >= 0 && b >= 0) { return order.slice(Math.min(a, b), Math.max(a, b) + 1); }
    }
    if (e.ctrlKey || e.metaKey) { return toggleKey(keys, key); }
    return [key];
  }
  return null;
}

export function toggleKey(keys: readonly string[], key: string): string[] {
  return keys.indexOf(key) >= 0 ? keys.filter((k) => k !== key) : keys.concat([key]);
}

/** Enter / Space on a row. */
export function keySelect(mode: SelectionMode | null, keys: string[], key: string): string[] | null {
  if (mode === "single") { return keys.length === 1 && keys[0] === key ? [] : [key]; }
  if (mode === "multiple" || mode === "checkbox") { return toggleKey(keys, key); }
  return null;
}

export function markRows(rows: readonly HTMLElement[], pre: string, keys: readonly string[],
                         mode: SelectionMode | null): void {
  rows.forEach((tr) => {
    const on = keys.indexOf(tr.getAttribute("data-key") as string) >= 0;
    tr.classList.toggle(pre + "-row-selected", on);
    if (mode !== "none") { tr.setAttribute("aria-selected", String(on)); }
    kids(tr, "td").forEach((td) => {
      kids<HTMLInputElement>(td, "." + pre + "-row-checkbox").forEach((cb) => { cb.checked = on; });
    });
  });
}

export function headerCheck(box: HTMLInputElement | null, keys: readonly string[],
                            visibleKeys: readonly string[]): void {
  if (!box) { return; }
  const n = visibleKeys.filter((k) => keys.indexOf(k) >= 0).length;
  box.checked = n > 0 && n === visibleKeys.length;
  box.indeterminate = n > 0 && n < visibleKeys.length;
}

/** Roving tabindex: one row of the table is in the tab order. */
export function focusRow(rows: readonly HTMLElement[], tr: HTMLElement | null | undefined, move: boolean): void {
  if (!tr) { return; }
  rows.forEach((r) => { r.setAttribute("tabindex", "-1"); });
  tr.setAttribute("tabindex", "0");
  if (move) {
    tr.focus();
    if (tr.scrollIntoView) { tr.scrollIntoView({ block: "nearest" }); }
  }
}

/** The header / body part of the table (null when missing). */
export function part(el: Element, pre: string, name: string): HTMLElement | null {
  return kid(kid(el, "." + pre + "-content"), "." + pre + "-" + name);
}

/** The table of a part (header or body). */
export function partTable(el: Element, pre: string, name: string): HTMLTableElement | null {
  return kid<HTMLTableElement>(part(el, pre, name), "table");
}

/** Sort state after a header click: asc -> desc -> none. */
export function nextSort(el: Element, field: string | null): SortDir {
  const cur = el.getAttribute("data-sort-field"), dir = el.getAttribute("data-sort-dir");
  if (cur !== field) { return "asc"; }
  return dir === "asc" ? "desc" : (dir === "desc" ? null : "asc");
}

/** A direction from the server or the root: anything else is unsorted. */
export function toDir(v: unknown): SortDir { return v === "asc" || v === "desc" ? v : null; }

export function writeSort(el: Element, pre: string, field: string | null, dir: SortDir): void {
  if (field && dir) {
    el.setAttribute("data-sort-field", field);
    el.setAttribute("data-sort-dir", dir);
  } else {
    el.removeAttribute("data-sort-field");
    el.removeAttribute("data-sort-dir");
  }
  const thead = kid(partTable(el, pre, "header"), "thead");
  kids(thead, "tr").forEach((tr) => {
    kids(tr, "th[data-field]").forEach((th) => {
      const on = !!dir && th.getAttribute("data-field") === field;
      th.classList.toggle(pre + "-sort-asc", on && dir === "asc");
      th.classList.toggle(pre + "-sort-desc", on && dir === "desc");
      if (on) { th.setAttribute("aria-sort", dir === "asc" ? "ascending" : "descending"); }
      else { th.removeAttribute("aria-sort"); }
    });
  });
}

export function isOff(el: Element): boolean { return el.getAttribute("aria-disabled") === "true"; }

/** Elements inside a cell that keep their own clicks. */
export const OWN = "a, button, input, select, textarea, label";

/**
 * The frame of a split header / body table (prefix pre, "ah-tg" or
 * "ah-dt"): the body's scroll moves the header along, the header leaves
 * room for the body's scrollbar, and dragging a header edge resizes a
 * column in both tables. bind() in setup, stop() in teardown.
 */
export class TableFrame {
  readonly #ctl: Controller;
  readonly #pre: string;
  #ro: ResizeObserver | null = null;
  #stopResize: (() => void) | null = null;

  constructor(ctl: Controller, pre: string) {
    this.#ctl = ctl;
    this.#pre = pre;
  }

  bind(): void {
    this.#bindScroll();
    this.#bindResize();
    this.#watchGutter();
  }

  stop(): void {
    if (this.#stopResize) { this.#stopResize(); }
    if (this.#ro) { this.#ro.disconnect(); }
  }

  // Body scroll moves the header along. Scroll does not bubble: the
  // listener captures on the root, so a body the server morphed in keeps
  // working.
  #bindScroll(): void {
    const el = this.#ctl.element, pre = this.#pre;
    this.#ctl.listen(el, "scroll", (e) => {
      const body = part(el, pre, "body");
      if (!body || e.target !== body) { return; }
      const header = part(el, pre, "header");
      if (header) { header.scrollLeft = body.scrollLeft; }
    }, { capture: true });
  }

  // The header leaves room for the body's vertical scrollbar, so the
  // columns of both tables line up (sigil reserves it with
  // scrollbar-gutter, which leaves an empty strip when nothing scrolls).
  #syncGutter(): void {
    const el = this.#ctl.element, pre = this.#pre;
    const body = part(el, pre, "body"), header = part(el, pre, "header");
    if (!body || !header) { return; }
    const w = body.offsetWidth - body.clientWidth;
    header.style.paddingRight = w > 0 ? w + "px" : "";
  }

  #watchGutter(): void {
    this.#syncGutter();
    const body = part(this.#ctl.element, this.#pre, "body");
    if (body && window.ResizeObserver) {
      const ro = this.#ro = new ResizeObserver(() => { this.#syncGutter(); });
      ro.observe(body);
      const table = kid(body, "table");
      if (table) { ro.observe(table); }
    }
  }

  // Dragging a header edge resizes the column in both tables; fires
  // ah:column-resize (detail {field, width}) on the root.
  #bindResize(): void {
    const ctl = this.#ctl, el = ctl.element, pre = this.#pre;
    ctl.delegate("mousedown", "." + pre + "-resize-handle", (e, handle) => {
      if (e.button !== 0) { return; }
      e.preventDefault();
      e.stopPropagation();
      // the handle sits in its header cell (server markup)
      const th = handle.parentElement as HTMLElement;
      const idx = Array.prototype.indexOf.call((th.parentElement as HTMLElement).children, th);
      const cols: HTMLElement[] = [];
      [partTable(el, pre, "header"), partTable(el, pre, "body")].forEach((t) => {
        const g = kid(t, "colgroup");
        const c = g ? g.children[idx] : undefined;
        if (c instanceof HTMLElement) { cols.push(c); }
      });
      const x0 = e.pageX, w0 = th.getBoundingClientRect().width;
      let w = w0;
      const stop = new AbortController();
      const move = (ev: MouseEvent): void => {
        w = Math.max(40, Math.round(w0 + ev.pageX - x0));
        cols.forEach((c) => { c.style.width = w + "px"; c.style.minWidth = w + "px"; });
      };
      const up = (): void => {
        stop.abort();
        this.#stopResize = null;
        ctl.fire<ColumnResizeEvent>("ah:column-resize", { field: th.getAttribute("data-field"), width: w });
      };
      document.addEventListener("mousemove", move, { signal: stop.signal });
      document.addEventListener("mouseup", up, { signal: stop.signal });
      this.#stopResize = () => { stop.abort(); };
    });
  }
}
