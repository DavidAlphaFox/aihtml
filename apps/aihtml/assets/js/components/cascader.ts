/* Behaviour of cascader (designs/04-components.md), ported from sigil's
 * form/cascader. The columns and the search list are rendered on the
 * server (aihtml_cascader); the controller shows, hides and marks them.
 * Value contract: data-ah-value (the path, a value list), the hidden
 * input and a native "change" on the root. Events on the root: "ah:open",
 * "ah:close" (no detail). A lazy branch fires "ah:load" (no detail) on
 * the .ah-cascader-loader child, whose data-ah-value is the path; an
 * "ah:error" on the loader drops the loading message.
 * Shared helpers: _lib_list.ts. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { ensureId, publish, split, join, shown, enabled, scrollInto, kids, childText, highlight } from "./_lib_list.ts";

/** A lazy branch waiting for its column (childrenLoaded). */
interface Pending {
  path: string;
  kbd: boolean;
}

function item(col: Element | null, v: string | undefined): HTMLElement | null {
  if (!col) { return null; }
  return Array.from(col.querySelectorAll<HTMLElement>("li[data-value]"))
    .filter((li) => li.getAttribute("data-value") === v)[0] || null;
}

function label(li: Element): string { return childText(li, ".ah-cascader-menu-item-label"); }

function pathOf(li: Element): string[] {
  const col = li.closest(".ah-cascader-menu-column");
  return split(col ? col.getAttribute("data-parent") : null).concat([li.getAttribute("data-value") || ""]);
}

function isBranch(li: Element): boolean { return li.classList.contains("has-children"); }

function rowsOf(col: Element | null): HTMLElement[] {
  return col ? Array.from(col.querySelectorAll<HTMLElement>("li[data-value]")).filter(enabled) : [];
}

function leaf(li: Element): void {
  li.classList.remove("has-children");
  li.removeAttribute("data-lazy");
  li.removeAttribute("aria-haspopup");
  kids(li, ".ah-cascader-menu-item-arrow").forEach((a) => { a.remove(); });
}

function removeAll(nodes: readonly Element[]): void { nodes.forEach((n) => { n.remove(); }); }

function level(col: Element | null): number {
  return col ? parseInt(col.getAttribute("data-level") || "", 10) || 0 : 0;
}

class CascaderController extends AH.Controller {
  // parts of the server markup (aihtml_cascader), found in setup
  private input!: HTMLInputElement;
  private clearBtn: HTMLElement | null = null;
  private popup!: HTMLElement;
  private menus!: HTMLElement;
  private search: HTMLElement | null = null;
  private loader: HTMLElement | null = null;
  // settings
  private sep = " / ";
  private emptyText = AH.t("common", "no_results", "No results found");
  private cos = false;
  private filterable = false;
  // state
  private value: string[] = [];
  private openPath: string[] = [];
  private isOpen = false;
  private cursor: HTMLElement | null = null;
  private query = "";
  private searchActive = -1;
  private pending: Pending | null = null;
  private display = "";
  private float: FloatHandle | null = null;

  private readonly onDocDown = (e: MouseEvent): void => {
    const t = e.target as Node | null;
    if (t && t.isConnected !== false && !this.element.contains(t)) { this.close(); }
  };

  override setup(): void {
    const el = this.element;
    ensureId(el, "ah-cs");
    // the text field, the popup and its menus are always rendered
    this.input = el.querySelector<HTMLInputElement>("input.ah-cascader-input") as HTMLInputElement;
    this.clearBtn = el.querySelector<HTMLElement>(".ah-cascader-clear");
    this.popup = kids(el, ".ah-cascader-popup")[0] as HTMLElement;
    this.menus = kids(this.popup, ".ah-cascader-menus")[0] as HTMLElement;
    this.search = kids(this.popup, ".ah-cascader-search-panel")[0] || null;
    this.loader = kids(el, ".ah-cascader-loader")[0] || null;
    this.sep = el.getAttribute("data-ah-separator") || " / ";
    this.emptyText = el.getAttribute("data-ah-empty") || AH.t("common", "no_results", "No results found");
    this.cos = el.hasAttribute("data-ah-change-on-select");
    this.filterable = el.classList.contains("ah-cascader-filterable");
    this.value = split(el.getAttribute("data-ah-value"));
    this.openPath = []; this.isOpen = false; this.cursor = null;
    this.query = ""; this.searchActive = -1; this.pending = null;
    this.display = String(this.input.value);
    const input = this.input;
    this.listen(input, "focus", () => { el.classList.add("ah-cascader-focused"); });
    this.listen(input, "blur", () => {
      el.classList.remove("ah-cascader-focused");
      setTimeout(() => {
        if (document.activeElement !== input) { this.close(); }
      }, 150);
    });
    this.listen(input, "click", (e) => {
      e.preventDefault();
      if (this.isOpen && !this.filterable) { this.close(); } else { this.open(); }
    });
    this.listen(input, "input", () => {
      if (!this.filterable) { return; }
      this.open();
      this.runQuery(String(input.value));
    });
    this.listen(input, "keydown", (e) => { this.key(e); });
    // the text field is internal: only the root reports changes
    this.listen(input, "change", (e) => { e.stopPropagation(); });
    this.delegate("mousedown", ".ah-cascader-arrow, .ah-cascader-clear", (e) => {
      e.preventDefault();
    });
    this.delegate("click", ".ah-cascader-arrow", (e) => {
      e.preventDefault();
      input.focus();
      if (this.isOpen) { this.close(); } else { this.open(); }
    });
    this.delegate("click", ".ah-cascader-clear", (e) => {
      e.preventDefault();
      e.stopPropagation();
      if (this.blocked()) { return; }
      this.set([], true);
      this.close();
    });
    // the loader's request failed: drop the loading message
    if (this.loader) {
      this.listen(this.loader, "ah:error", () => {
        this.pending = null;
        this.dropLoading();
      });
    }
    this.listen(this.popup, "mousedown", (e) => { e.preventDefault(); });
    this.delegate("click", ".ah-cascader-menu li[data-value]", (e, li) => {
      e.preventDefault();
      this.choose(li, false);
    }, this.popup);
    this.delegate("click", ".ah-cascader-search-item", (_e, li) => { this.searchPick(li); }, this.popup);
  }

  override teardown(): void {
    if (this.float) { this.float.stop(); this.float = null; }
    document.removeEventListener("mousedown", this.onDocDown);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  /** Called by aihtml_cascader:cascader_children/3 after it appended
   *  the column(s) of `path`; no column means the node is a leaf. */
  childrenLoaded(path: string | readonly string[]): void {
    const p = split(path);
    const key = join(p);
    const pending = this.pending && this.pending.path === key ? this.pending : null;
    if (pending) { this.pending = null; this.dropLoading(); }
    const cols = this.columns().filter((c) => c.getAttribute("data-parent") === key);
    removeAll(cols.slice(0, -1));
    const li = item(this.column(p.slice(0, -1)), p[p.length - 1]);
    if (li) { li.removeAttribute("data-lazy"); }
    if (!cols.length) {
      if (li) { leaf(li); }
      if (pending && this.isOpen && li) { this.choose(li, pending.kbd); }
      return;
    }
    if (this.isOpen && join(this.openPath) === key) {
      this.show();
      if (pending && pending.kbd) { this.moveCursor(rowsOf(cols[cols.length - 1])[0]); }
    }
  }
  /** A path "a,b,c" or ["a", "b", "c"]; no change event. */
  setValue(v: string | readonly string[]): void { this.set(split(v), false); }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  getLabels(): string[] { return this.labels(this.value); }
  clear(): void { this.set([], true); }

  open(): void {
    if (this.isOpen || this.blocked()) { return; }
    this.isOpen = true;
    this.openPath = this.value.slice();
    this.popup.classList.add("ah-cascader-popup-open");
    this.show();
    const v = this.value;
    const last = v.length ? item(this.column(v.slice(0, -1)), v[v.length - 1]) : null;
    this.moveCursor(last || rowsOf(this.column([]))[0]);
    this.element.classList.add("ah-cascader-open");
    this.input.setAttribute("aria-expanded", "true");
    document.addEventListener("mousedown", this.onDocDown);
    this.fire("ah:open");
  }

  close(): void {
    if (!this.isOpen) { return; }
    this.isOpen = false;
    this.popup.classList.remove("ah-cascader-popup-open");
    if (this.float) { this.float.stop(); this.float = null; }
    this.element.classList.remove("ah-cascader-open");
    this.input.setAttribute("aria-expanded", "false");
    this.moveCursor(null);
    this.dropLoading();
    if (this.query) { this.runQuery(""); }
    this.input.value = this.display;
    document.removeEventListener("mousedown", this.onDocDown);
    this.fire("ah:close");
  }

  private blocked(): boolean { return this.element.classList.contains("ah-cascader-disabled"); }

  private columns(): HTMLElement[] { return kids(this.menus, ".ah-cascader-menu-column"); }

  // The column holding the children of `path` (an array); the last one
  // wins when a lazy level was loaded twice.
  private column(path: readonly string[]): HTMLElement | null {
    const key = join(path);
    const cols = this.columns().filter((c) => c.getAttribute("data-parent") === key);
    return cols[cols.length - 1] || null;
  }

  // The labels along a path; values without a row show as themselves.
  private labels(path: readonly string[]): string[] {
    const out: string[] = [];
    for (let i = 0; i < path.length; i++) {
      const li = item(this.column(path.slice(0, i)), path[i]);
      out.push(li ? label(li) : path[i]);
    }
    return out;
  }

  private dropLoading(): void { removeAll(kids(this.menus, ".ah-cascader-loading")); }

  // Show the columns of the open path and mark its rows active.
  private show(): void {
    this.columns().forEach((c) => { c.setAttribute("hidden", "hidden"); });
    this.menus.querySelectorAll("li.active").forEach((li) => {
      li.classList.remove("active");
      li.setAttribute("aria-selected", "false");
    });
    let col = this.column([]);
    for (let i = 0; col; i++) {
      col.removeAttribute("hidden");
      const li = i < this.openPath.length ? item(col, this.openPath[i]) : null;
      if (!li) { break; }
      li.classList.add("active");
      li.setAttribute("aria-selected", "true");
      if (!isBranch(li)) { break; }
      col = this.column(this.openPath.slice(0, i + 1));
    }
    this.position();
  }

  private moveCursor(li: HTMLElement | null | undefined): void {
    this.menus.querySelectorAll(".ah-cascader-menu-item-focused").forEach((x) => {
      x.classList.remove("ah-cascader-menu-item-focused");
    });
    this.cursor = li || null;
    if (!li) { this.input.removeAttribute("aria-activedescendant"); return; }
    ensureId(li, this.element.id + "-o");
    li.classList.add("ah-cascader-menu-item-focused");
    this.input.setAttribute("aria-activedescendant", li.id);
    scrollInto(li.closest<HTMLElement>(".ah-cascader-menu"), li);
  }

  private position(): void {
    if (!this.isOpen) { return; }
    if (this.float) { this.float.update(); } else { this.float = AH.float(this.popup, this.element); }
  }

  private set(path: readonly string[], fire: boolean): void {
    this.value = path.slice();
    this.display = this.labels(path).join(this.sep);
    if (!this.query) { this.input.value = this.display; }
    if (this.clearBtn) { this.clearBtn.hidden = !path.length; }
    publish(this.element, join(path), fire);
  }

  // Open a branch: its column (loaded or not), or a leaf: pick it.
  private choose(li: HTMLElement | null | undefined, kbd: boolean): void {
    if (!li || !enabled(li)) { return; }
    const path = pathOf(li);
    if (!isBranch(li)) {
      this.set(path, true);
      this.close();
      return;
    }
    this.openPath = path;
    if (this.cos) { this.set(path, true); }
    const col = this.column(path);
    this.dropLoading();
    if (col) {
      this.show();
      this.moveCursor(kbd ? rowsOf(col)[0] : li);
    } else if (li.hasAttribute("data-lazy") && this.loader) {
      this.show();
      this.moveCursor(li);
      this.pending = { path: join(path), kbd: kbd };
      const loading = document.createElement("div");
      loading.className = "ah-cascader-loading";
      loading.textContent = AH.t("common", "loading_items", "Loading…");
      this.menus.appendChild(loading);
      this.position();
      this.loader.setAttribute("data-ah-value", join(path));
      this.fire("ah:load", undefined, this.loader);
    } else {
      leaf(li);
      this.choose(li, kbd);
    }
  }

  private moveBy(dir: number, edge?: boolean): void {
    const col = this.cursor ? this.cursor.closest(".ah-cascader-menu-column") : this.column([]);
    const rows = rowsOf(col);
    if (!rows.length) { return; }
    let i = this.cursor ? rows.indexOf(this.cursor) : -1;
    if (edge) { i = dir > 0 ? rows.length - 1 : 0; } else if (i < 0) { i = 0; } else {
      i = (i + dir + rows.length) % rows.length;
    }
    // moving within a column closes the columns to its right
    const lv = level(col);
    if (this.openPath.length > lv) { this.openPath = this.openPath.slice(0, lv); this.show(); }
    this.moveCursor(rows[i]);
  }

  // Search (filterable): the server-rendered path list, filtered here.
  private runQuery(q: string): void {
    this.query = q;
    removeAll(kids(this.popup, ".ah-cascader-empty"));
    if (!q) {
      if (this.search) { this.search.setAttribute("hidden", "hidden"); }
      this.menus.removeAttribute("hidden");
      this.searchActive = -1;
      this.position();
      return;
    }
    this.menus.setAttribute("hidden", "hidden");
    let some = false;
    if (this.search) {
      this.search.removeAttribute("hidden");
      const lower = q.toLowerCase();
      kids(this.search, "li").forEach((li) => {
        const text = li.getAttribute("data-label") || "";
        const hit = text.toLowerCase().indexOf(lower) >= 0;
        li.style.display = hit ? "" : "none";
        if (hit) { some = true; highlight(li.firstChild, text, q); }
      });
    }
    if (!some) {
      if (this.search) { this.search.setAttribute("hidden", "hidden"); }
      const empty = document.createElement("div");
      empty.className = "ah-cascader-empty";
      empty.textContent = this.emptyText;
      this.popup.appendChild(empty);
    }
    this.setSearchActive(-1);
    this.position();
  }

  private searchRows(): HTMLElement[] {
    return kids(this.search, "li").filter((li) => shown(li) && enabled(li));
  }

  private setSearchActive(i: number): void {
    const rows = this.searchRows();
    kids(this.search, ".active").forEach((li) => {
      li.classList.remove("active");
      li.setAttribute("aria-selected", "false");
    });
    const row = rows[i];
    this.searchActive = row ? i : -1;
    if (!row) { this.input.removeAttribute("aria-activedescendant"); return; }
    ensureId(row, this.element.id + "-s");
    row.classList.add("active");
    row.setAttribute("aria-selected", "true");
    this.input.setAttribute("aria-activedescendant", row.id);
    scrollInto(this.search, row);
  }

  private searchPick(li: HTMLElement | null | undefined): void {
    if (!li || !enabled(li)) { return; }
    this.query = "";
    this.set(split(li.getAttribute("data-path")), true);
    this.close();
  }

  private key(e: KeyboardEvent): void {
    if (this.blocked()) { return; }
    const k = e.key;
    if (!this.isOpen) {
      if (k === "ArrowDown" || k === "ArrowUp" || k === "Enter" || (k === " " && !this.filterable)) {
        e.preventDefault();
        this.open();
      }
      return;
    }
    if (this.query) {
      const rows = this.searchRows();
      switch (k) {
        case "ArrowDown": e.preventDefault(); this.setSearchActive((this.searchActive + 1) % Math.max(rows.length, 1)); return;
        case "ArrowUp": e.preventDefault(); this.setSearchActive(this.searchActive <= 0 ? rows.length - 1 : this.searchActive - 1); return;
        case "Enter": e.preventDefault(); this.searchPick(rows[this.searchActive] || (rows.length === 1 ? rows[0] : null)); return;
        case "Escape": e.preventDefault(); this.input.value = ""; this.runQuery(""); return;
        case "Tab": this.close(); return;
        default: return;
      }
    }
    switch (k) {
      case "ArrowDown": e.preventDefault(); this.moveBy(1); break;
      case "ArrowUp": e.preventDefault(); this.moveBy(-1); break;
      case "Home": if (!this.filterable) { e.preventDefault(); this.moveBy(-1, true); } break;
      case "End": if (!this.filterable) { e.preventDefault(); this.moveBy(1, true); } break;
      case "ArrowRight":
        if (this.cursor && isBranch(this.cursor)) { e.preventDefault(); this.choose(this.cursor, true); }
        break;
      case "ArrowLeft": {
        const col = this.cursor ? this.cursor.closest(".ah-cascader-menu-column") : null;
        if (col && level(col) > 0) {
          e.preventDefault();
          const parent = split(col.getAttribute("data-parent"));
          this.openPath = parent.slice(0, -1);
          this.show();
          this.moveCursor(item(this.column(parent.slice(0, -1)), parent[parent.length - 1]));
        }
        break;
      }
      case " ":
        if (this.filterable) { break; }
        e.preventDefault(); this.choose(this.cursor, true); break;
      case "Enter": e.preventDefault(); this.choose(this.cursor, true); break;
      case "Escape": e.preventDefault(); this.close(); break;
      case "Tab": this.close(); break;
      default: break;
    }
  }
}

AH.register("cascader", CascaderController);
