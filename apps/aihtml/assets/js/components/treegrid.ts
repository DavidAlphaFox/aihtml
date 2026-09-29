/* Controller of the tree grid (designs/04-components.md).
 *
 * Ported from sigil (data/treegrid). The server renders every row and
 * header; this file moves state around in that DOM: expand / collapse
 * (rows under a closed node are hidden), sorting within each level,
 * selection (single, multiple with Ctrl / Shift, checkbox), keyboard
 * (treegrid pattern: roving tabindex over rows, arrows, Home/End,
 * Enter/Space), lazy rows loaded through the load action (data-load, a
 * signed token) which answers with treegrid_children/3 -> childrenLoaded.
 * Shared helpers are in _lib_table.ts.
 *
 * The view state lives in the DOM (root data-* attributes, row
 * attributes), so a morph keeps it. The selection is the root's
 * data-ah-value (keys joined with commas), mirrored into a hidden input;
 * "change" fires when the user changes it, never from methods.
 *
 * Events (native CustomEvents, bubbling; the detail in e.detail):
 *   ah:expand, ah:collapse, ah:row-click, ah:row-dblclick  {key, level}
 *                     (the key is also the root's data-key)
 *   ah:load           {key, level}, fired on the lazy row
 *   ah:sort           {field, dir}   (SortEvent)
 *   ah:column-resize  {field, width} (ColumnResizeEvent)
 */
import AH from "../core.ts";
import {
  OWN, TableFrame, clickSelect, compare, focusRow, headerCheck, isOff, keySelect, keysOf, kid, kids,
  markRows, nextSort, ord, part, raw, rowKey, sameKeys, toDir, toKeys, writeSort, writeValue
} from "./_lib_table.ts";
import type { SortDir, SortEvent } from "./_lib_table.ts";

export type { SortEvent, ColumnResizeEvent } from "./_lib_table.ts";

/** Detail of ah:expand, ah:collapse, ah:row-click, ah:row-dblclick, ah:load. */
export interface TreeRowEvent { key: string; level: number; }

type ToggleState = "open" | "closed" | "leaf";

const ROW = "tbody > tr.ah-tg-row";

function isOpen(tr: Element): boolean { return tr.getAttribute("aria-expanded") === "true"; }

function info(tr: Element): TreeRowEvent {
  return { key: rowKey(tr), level: parseInt(tr.getAttribute("data-level") || "", 10) };
}

function setToggleIcon(tr: Element, state: ToggleState): void {
  kids(tr, "td").forEach((td) => {
    kids(td, ".ah-tg-tree-indent").forEach((ind) => {
      kids(ind, ".ah-tg-toggle").forEach((t) => {
        t.classList.remove("ah-tg-toggle-open", "ah-tg-toggle-closed", "ah-tg-toggle-leaf");
        t.classList.add("ah-tg-toggle-" + state);
        t.textContent = state === "leaf" ? "" : "▶";
      });
    });
  });
}

class TreegridController extends AH.Controller {
  #anchor: string | null = null;
  readonly #frame = new TableFrame(this, "ah-tg");

  override setup(): void {
    const el = this.element;
    this.#anchor = null;
    this.delegate<MouseEvent, HTMLTableRowElement>("click", ROW, (e, tr) => { this.#click(e, tr); });
    this.delegate<MouseEvent, HTMLTableRowElement>("dblclick", ROW, (e, tr) => {
      const t = e.target as Element;
      if (tr.parentNode === this.#body && !t.closest(".ah-tg-toggle")) {
        this.#event("ah:row-dblclick", tr);
      }
    });
    this.listen(el, "keydown", (e) => {
      if (!(e.target instanceof Element)) { return; }
      const tr = e.target.closest<HTMLTableRowElement>(ROW);
      if (tr && el.contains(tr)) { this.#keydown(e, tr); return; }
      const th = e.target.closest<HTMLElement>(".ah-tg-th-sortable");
      if (th && el.contains(th) && (e.key === "Enter" || e.key === " ")) {
        e.preventDefault();
        th.click();
      }
    });
    // Inner controls (row / header boxes, filters, pager, chooser) report
    // to the table, not as its own change: stopImmediatePropagation also
    // keeps them from listeners on the root added after this one, as
    // jQuery's delegated stopPropagation did.
    this.listen(el, "change", (e) => {
      const t = e.target;
      if (!(t instanceof HTMLInputElement)) { return; }
      if (t.matches(".ah-tg-row-checkbox")) {
        e.stopImmediatePropagation();
        // a row box sits in its row (server markup)
        const k = rowKey(t.closest("tr") as HTMLTableRowElement);
        const rest = keysOf(el).filter((x) => x !== k);
        this.#select(t.checked ? rest.concat([k]) : rest, true);
      } else if (t.matches(".ah-tg-header-checkbox")) {
        e.stopImmediatePropagation();
        this.#select(t.checked ? this.#keys() : [], true);
      }
    });
    this.delegate<MouseEvent>("click", ".ah-tg-th-sortable", (e, th) => {
      if ((e.target as Element).closest(".ah-tg-resize-handle") || isOff(el)) { return; }
      const field = th.getAttribute("data-field");
      const dir = nextSort(el, field);
      this.#sort(dir ? field : null, dir);
      this.fire<SortEvent>("ah:sort", { field, dir });
    });
    this.#frame.bind();
    headerCheck(this.#headerBox(), keysOf(el), this.#keys());
    const rows = this.#rows;
    if (!rows.some((r) => r.getAttribute("tabindex") === "0")) {
      focusRow(rows, this.#visible()[0], false);
    }
  }

  override teardown(): void {
    this.#frame.stop();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v: unknown): void {
    this.#select(toKeys(v).filter((k) => this.#byKey(k)), false);
  }
  clearSelection(): void { this.#select([], false); }
  expand(k: unknown): void { this.#setOpen(this.#byKey(k), true); }
  collapse(k: unknown): void { this.#setOpen(this.#byKey(k), false); }
  toggle(k: unknown): void {
    const tr = this.#byKey(k);
    if (tr) { this.#setOpen(tr, !isOpen(tr)); }
  }
  expandAll(): void {
    this.#rows.forEach((r) => {
      if (r.hasAttribute("aria-expanded") && !r.hasAttribute("data-lazy")) {
        r.setAttribute("aria-expanded", "true");
        setToggleIcon(r, "open");
      }
    });
    this.#layout();
  }
  collapseAll(): void {
    this.#rows.forEach((r) => {
      if (r.hasAttribute("aria-expanded")) {
        r.setAttribute("aria-expanded", "false");
        setToggleIcon(r, "closed");
      }
    });
    this.#layout();
  }
  ensureVisible(k: unknown): void {
    const tr = this.#byKey(k);
    for (let p = tr && this.#parent(tr); p; p = this.#parent(p)) {
      if (!isOpen(p)) {
        p.setAttribute("aria-expanded", "true");
        setToggleIcon(p, "open");
      }
    }
    this.#layout();
    if (tr && tr.scrollIntoView) { tr.scrollIntoView({ block: "nearest" }); }
  }
  sort(field: unknown, dir?: unknown): void {
    const d = toDir(dir);
    this.#sort(d ? String(field) : null, d);
  }
  childrenLoaded(id: string): void {
    const tr = document.getElementById(id);
    if (tr) { this.#childrenLoaded(tr); }
  }

  // ---- rows ----------------------------------------------------------

  get #body(): HTMLTableSectionElement | null {
    return document.getElementById(this.element.id + "-rows") as HTMLTableSectionElement | null;
  }
  get #rows(): HTMLTableRowElement[] { return kids<HTMLTableRowElement>(this.#body, "tr.ah-tg-row"); }
  #visible(): HTMLTableRowElement[] { return this.#rows.filter((r) => !r.hidden); }
  #keys(): string[] { return this.#rows.map(rowKey); }
  #byKey(key: unknown): HTMLTableRowElement | null {
    if (key === null || key === undefined) { return null; }
    const k = String(key);
    return this.#rows.filter((r) => r.getAttribute("data-key") === k)[0] || null;
  }
  #parent(tr: Element): HTMLTableRowElement | null { return this.#byKey(tr.getAttribute("data-parent") || null); }
  #headerBox(): HTMLInputElement | null {
    const h = part(this.element, "ah-tg", "header");
    return h ? h.querySelector<HTMLInputElement>(".ah-tg-header-checkbox") : null;
  }

  // Rows show when every ancestor is open; stripes follow the shown rows.
  #layout(): void {
    const el = this.element;
    const open: Record<string, boolean> = {};
    const alt = el.getAttribute("data-alt-rows") === "true";
    let n = 0;
    const rows = this.#rows;
    const stop = rows.filter((r) => r.getAttribute("tabindex") === "0")[0];
    const hadFocus = !!stop && document.activeElement === stop;
    rows.forEach((r) => {
      const p = r.getAttribute("data-parent");
      const shown = !p || !(p in open) || open[p];
      r.hidden = !shown;
      open[rowKey(r)] = shown && isOpen(r);
      if (shown) {
        r.classList.toggle("ah-tg-row-alt", alt && n % 2 === 1);
        n++;
      }
    });
    kids(this.#body, "tr.ah-tg-row-empty").forEach((r) => { r.hidden = rows.length > 0; });
    // the tab stop never stays on a hidden row
    if (!stop || stop.hidden) {
      let to: HTMLTableRowElement | null | undefined = stop;
      while (to && to.hidden) { to = this.#parent(to); }
      focusRow(rows, to || this.#visible()[0], hadFocus);
    }
  }

  #event(name: string, tr: Element): void {
    const d = info(tr);
    this.element.setAttribute("data-key", d.key);
    this.fire<TreeRowEvent>(name, d);
  }

  #setOpen(tr: HTMLTableRowElement | null, open: boolean): void {
    if (!tr || !tr.hasAttribute("aria-expanded") || isOpen(tr) === open) { return; }
    if (open && tr.getAttribute("data-lazy") === "true") {
      this.#load(tr);
      return;
    }
    tr.setAttribute("aria-expanded", String(open));
    setToggleIcon(tr, open ? "open" : "closed");
    this.#layout();
    this.#event(open ? "ah:expand" : "ah:collapse", tr);
  }

  // A lazy row asks the server for its children: the load token is bound
  // to the row (ah:load), so each row has its own request.
  #load(tr: HTMLTableRowElement): void {
    const el = this.element;
    const token = el.getAttribute("data-load");
    if (!token || tr.classList.contains("ah-tg-row-loading")) { return; }
    tr.classList.add("ah-tg-row-loading");
    tr.setAttribute("aria-busy", "true");
    tr.setAttribute("data-value", el.getAttribute("data-ah-value") || "");
    if (!tr.hasAttribute("data-ah-on")) {
      tr.setAttribute("data-ah-on", "ah:load:" + token);
      AH.mount(tr);                   // registers the ah:load listener
    }
    this.fire<TreeRowEvent>("ah:load", info(tr), tr);
  }

  #childrenLoaded(tr: HTMLElement): void {
    const el = this.element;
    const prefix = tr.id + "-";
    let after: HTMLElement = tr;
    this.#rows.filter((r) => r.id.indexOf(prefix) === 0).forEach((r) => {
      after.after(r);
      after = r;
    });
    tr.classList.remove("ah-tg-row-loading");
    ["aria-busy", "data-lazy", "data-ah-on", "data-value"].forEach((a) => { tr.removeAttribute(a); });
    if (after === tr) {
      tr.removeAttribute("aria-expanded");
      tr.classList.add("ah-tg-row-leaf");
      setToggleIcon(tr, "leaf");
      this.#layout();
      return;
    }
    tr.setAttribute("aria-expanded", "true");
    setToggleIcon(tr, "open");
    const f = el.getAttribute("data-sort-field");
    if (f) { this.#sort(f, toDir(el.getAttribute("data-sort-dir"))); }
    markRows(this.#rows, "ah-tg", keysOf(el), el.getAttribute("data-selection"));
    this.#layout();
    this.#event("ah:expand", tr);
  }

  // Siblings in the order of a column (or the original order), then the
  // rows re-laid depth first.
  #sort(field: string | null, dir: SortDir): void {
    const rows = this.#rows;
    const keys: Record<string, boolean> = {};
    rows.forEach((r) => { keys[rowKey(r)] = true; });
    const byParent: Record<string, HTMLTableRowElement[]> = { "": [] };
    rows.forEach((r) => {
      let p = r.getAttribute("data-parent") || "";
      if (!keys[p]) { p = ""; }
      (byParent[p] = byParent[p] || []).push(r);
    });
    const sign = dir === "desc" ? -1 : 1;
    const cell = (r: Element): string =>
      raw(kids(r, "td[data-field]").filter((td) => td.getAttribute("data-field") === field)[0]);
    Object.keys(byParent).forEach((p) => {
      byParent[p].sort((a, b) => {
        const c = dir && field ? sign * compare(cell(a), cell(b)) : 0;
        return c || ord(a) - ord(b);
      });
    });
    const body = this.#body;
    if (body) {
      const end = kid(body, "tr.ah-tg-row-empty");
      const walk = (list: readonly HTMLTableRowElement[]): void => {
        list.forEach((r) => {
          body.insertBefore(r, end);
          walk(byParent[rowKey(r)] || []);
        });
      };
      walk(byParent[""]);
    }
    writeSort(this.element, "ah-tg", field, dir);
    this.#layout();
  }

  // ---- selection and input ------------------------------------------

  #select(keys: string[], user: boolean): void {
    const el = this.element;
    const prev = keysOf(el);
    markRows(this.#rows, "ah-tg", keys, el.getAttribute("data-selection"));
    writeValue(el, keys);
    headerCheck(this.#headerBox(), keys, this.#keys());
    if (user && !sameKeys(prev, keys)) { this.fire("change"); }
  }

  #keydown(e: KeyboardEvent, tr: HTMLTableRowElement): void {
    const el = this.element;
    if (e.target !== tr || tr.parentNode !== this.#body || isOff(el) || e.altKey || e.metaKey) { return; }
    const vis = this.#visible();
    const i = vis.indexOf(tr);
    let to: HTMLTableRowElement | null | undefined = null;
    const key = rowKey(tr);
    switch (e.key) {
      case "ArrowDown": to = vis[Math.min(i + 1, vis.length - 1)]; break;
      case "ArrowUp": to = vis[Math.max(i - 1, 0)]; break;
      case "Home": to = vis[0]; break;
      case "End": to = vis[vis.length - 1]; break;
      case "PageDown": to = vis[Math.min(i + 10, vis.length - 1)]; break;
      case "PageUp": to = vis[Math.max(i - 10, 0)]; break;
      case "ArrowRight":
        if (tr.hasAttribute("aria-expanded") && !isOpen(tr)) { this.#setOpen(tr, true); }
        else if (isOpen(tr) && vis[i + 1] && vis[i + 1].getAttribute("data-parent") === key) { to = vis[i + 1]; }
        break;
      case "ArrowLeft":
        if (isOpen(tr)) { this.#setOpen(tr, false); } else { to = this.#parent(tr); }
        break;
      case "Enter":
      case " ": {
        const next = keySelect(el.getAttribute("data-selection"), keysOf(el), key);
        if (next) { this.#select(next, true); this.#anchor = key; }
        break;
      }
      default:
        return;
    }
    e.preventDefault();
    if (to) { focusRow(this.#rows, to, true); }
  }

  #click(e: MouseEvent, tr: HTMLTableRowElement): void {
    const el = this.element;
    if (tr.parentNode !== this.#body || isOff(el)) { return; }
    const key = rowKey(tr);
    focusRow(this.#rows, tr, false);
    const t = e.target as Element;
    const toggle = t.closest(".ah-tg-toggle");
    if (toggle && tr.contains(toggle)) {
      this.#setOpen(tr, !isOpen(tr));
      return;
    }
    const own = t.closest(OWN);
    if (own && tr.contains(own)) { return; }
    const next = clickSelect(el.getAttribute("data-selection"), keysOf(el), key, e, this.#anchor,
                             this.#visible().map(rowKey));
    if (next) {
      this.#select(next, true);
      if (!e.shiftKey) { this.#anchor = key; }
    }
    this.#event("ah:row-click", tr);
  }
}

AH.register("treegrid", TreegridController);
