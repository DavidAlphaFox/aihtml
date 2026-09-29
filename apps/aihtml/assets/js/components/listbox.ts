/* Behaviour of listbox (designs/04-components.md), ported from sigil's
 * form/listbox. The rows are rendered on the server (aihtml_listbox);
 * the controller marks, filters and selects them. Value contract:
 * data-ah-value, the hidden input and a native "change" on the root.
 * Shared helpers: _lib_list.ts. */
import AH from "../core.ts";
import { ensureId, publish, split, join, shown, enabled, scrollInto, kids, childText } from "./_lib_list.ts";

/** The modifier keys of a click or a key press (none for type-ahead). */
interface Mods {
  ctrlKey?: boolean;
  metaKey?: boolean;
  shiftKey?: boolean;
}

const PAGE = 10;

function lbValue(li: Element): string { return li.getAttribute("data-value") || ""; }

class ListboxController extends AH.Controller {
  // parts of the server markup (aihtml_listbox), found in setup
  private list: HTMLElement | null = null;
  private content: HTMLElement | null = null;
  private empty: HTMLElement | null = null;
  private checkAll: HTMLElement | null = null;
  private filterInput: HTMLInputElement | null = null;
  // settings
  private checkboxes = false;
  private remote = false;
  private multi = false;
  // state
  private cursor: HTMLElement | null = null;
  private anchor: HTMLElement | null = null;
  private typed = "";
  private typedAt = 0;
  private selected: string[] = [];

  override setup(): void {
    const el = this.element;
    ensureId(el, "ah-lb");
    this.list = el.querySelector<HTMLElement>(".ah-listbox-list");
    this.content = kids(el, ".ah-listbox-content")[0] || null;
    this.empty = el.querySelector<HTMLElement>(".ah-listbox-empty");
    this.checkAll = kids(el, ".ah-listbox-check-all")[0] || null;
    this.filterInput = el.querySelector<HTMLInputElement>(".ah-listbox-filter-input");
    this.checkboxes = el.classList.contains("ah-listbox-checkboxes");
    this.remote = el.classList.contains("ah-listbox-remote");
    this.multi = this.checkboxes || el.classList.contains("ah-listbox-multiple");
    this.cursor = null; this.anchor = null; this.typed = ""; this.typedAt = 0;
    this.selected = split(el.getAttribute("data-ah-value"), !this.multi);
    const blocked = (): boolean => el.classList.contains("ah-listbox-disabled");
    this.delegate("mousedown", ".ah-listbox-item, .ah-listbox-check-all", (e) => {
      if (e.shiftKey) { e.preventDefault(); }  // no text selection on Shift+click
    });
    this.delegate("click", ".ah-listbox-item", (e, li) => {
      if (!blocked()) { this.clickRow(li, e); }
    });
    this.delegate("click", ".ah-listbox-check-all", () => {
      if (blocked()) { return; }
      const rows = this.rows().map(lbValue);
      const all = rows.length && rows.every((v) => this.selected.indexOf(v) >= 0);
      const rest = this.selected.filter((v) => rows.indexOf(v) < 0);
      this.set(all ? rest : rest.concat(rows), true);
    });
    this.listen(el, "keydown", (e) => { if (!blocked()) { this.key(e); } });
    this.listen(el, "focus", () => {
      if (!this.cursor) {
        const rows = this.rows();
        const sel = rows.filter((li) => this.selected.indexOf(lbValue(li)) >= 0)[0];
        if (sel || rows[0]) { this.moveCursor(sel || rows[0]); }
      }
    });
    const filterInput = this.filterInput;
    if (filterInput) {
      this.listen(filterInput, "input", () => { this.applyFilter(filterInput.value); });
      this.listen(filterInput, "change", (e) => { e.stopPropagation(); });
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  /** A value or a list (multiple); no change event (the server set it). */
  setValue(v: string | readonly string[] | null): void { this.set(split(v, !this.multi), false); }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  clear(): void { this.set([], true); }
  filter(text: string | null): void {
    if (this.filterInput) { this.filterInput.value = text == null ? "" : text; }
    this.applyFilter(text);
  }
  /** Called by aihtml_listbox:listbox_items/3 after it morphed the
   *  server-rendered rows into the list. */
  itemsLoaded(): void {
    this.anchor = null;
    this.groups();
  }

  private items(): HTMLElement[] { return kids(this.list, ".ah-listbox-item"); }
  // Rows keyboard and check-all work on: shown and enabled.
  private rows(): HTMLElement[] { return this.items().filter((li) => shown(li) && enabled(li)); }

  private mark(): void {
    this.items().forEach((li) => {
      const sel = this.selected.indexOf(lbValue(li)) >= 0;
      li.classList.toggle("ah-listbox-item-selected", sel);
      li.setAttribute("aria-selected", String(sel));
      kids(li, ".ah-listbox-checkbox").forEach((c) => {
        c.classList.toggle("ah-listbox-checkbox-checked", sel);
      });
    });
    if (this.checkAll) {
      const rows = this.rows();
      const n = rows.filter((li) => this.selected.indexOf(lbValue(li)) >= 0).length;
      const all = rows.length > 0 && n === rows.length;
      this.checkAll.setAttribute("aria-pressed", all ? "true" : (n ? "mixed" : "false"));
      kids(this.checkAll, ".ah-listbox-checkbox").forEach((c) => {
        c.classList.toggle("ah-listbox-checkbox-checked", all);
        c.classList.toggle("ah-listbox-checkbox-indeterminate", n > 0 && !all);
      });
    }
  }

  private set(values: readonly string[], fire: boolean): void {
    this.selected = this.multi ? values.slice() : values.slice(0, 1);
    this.mark();
    publish(this.element, join(this.selected, !this.multi), fire);
  }

  private moveCursor(li: HTMLElement | null | undefined): void {
    this.items().forEach((r) => { r.classList.remove("ah-listbox-item-focused"); });
    this.cursor = li || null;
    if (!li) { this.element.removeAttribute("aria-activedescendant"); return; }
    li.classList.add("ah-listbox-item-focused");
    this.element.setAttribute("aria-activedescendant", li.id);
    scrollInto(this.content, li);
  }

  private range(a: HTMLElement, b: HTMLElement): string[] {
    const rows = this.rows(), j = rows.indexOf(b);
    let i = rows.indexOf(a);
    if (i < 0) { i = j; }
    return rows.slice(Math.min(i, j), Math.max(i, j) + 1).map(lbValue);
  }

  private toggle(li: HTMLElement): void {
    const v = lbValue(li), next = this.selected.slice(), i = next.indexOf(v);
    if (i >= 0) { next.splice(i, 1); } else { next.push(v); }
    this.set(next, true);
  }

  // A click: sigil's select-item! (single, Ctrl toggle, Shift range) and
  // toggle-checkbox! (check boxes).
  private clickRow(li: HTMLElement, e: MouseEvent): void {
    if (!enabled(li)) { return; }
    if (this.checkboxes || (this.multi && (e.ctrlKey || e.metaKey))) {
      this.toggle(li);
      this.anchor = li;
    } else if (this.multi && e.shiftKey && this.anchor) {
      const add = this.range(this.anchor, li);
      this.set(this.selected.concat(add.filter((v) => this.selected.indexOf(v) < 0)), true);
    } else {
      this.set([lbValue(li)], true);
      this.anchor = li;
    }
    this.moveCursor(li);
  }

  // Arrow keys and friends: move the cursor and select like sigil (a
  // single row), or extend (Shift) or only move (Ctrl, check boxes).
  private go(li: HTMLElement | undefined, e: Mods): void {
    if (!li) { return; }
    const from = this.anchor || this.cursor || li;
    this.moveCursor(li);
    if (this.checkboxes || (this.multi && (e.ctrlKey || e.metaKey))) { return; }
    if (this.multi && e.shiftKey) {
      this.anchor = from;
      this.set(this.range(from, li), true);
      return;
    }
    this.anchor = li;
    this.set([lbValue(li)], true);
  }

  private key(e: KeyboardEvent): void {
    const inFilter = e.target !== this.element;
    const rows = this.rows();
    if (!rows.length) { return; }
    const i = this.cursor ? rows.indexOf(this.cursor) : -1;
    switch (e.key) {
      case "ArrowDown": e.preventDefault(); this.go(rows[Math.min(i + 1, rows.length - 1)], e); break;
      case "ArrowUp": e.preventDefault(); this.go(rows[Math.max(i - 1, 0)], e); break;
      case "PageDown": e.preventDefault(); this.go(rows[Math.min(Math.max(i, 0) + PAGE, rows.length - 1)], e); break;
      case "PageUp": e.preventDefault(); this.go(rows[Math.max(i - PAGE, 0)], e); break;
      case "Home": if (!inFilter) { e.preventDefault(); this.go(rows[0], e); } break;
      case "End": if (!inFilter) { e.preventDefault(); this.go(rows[rows.length - 1], e); } break;
      case " ":
        if (inFilter || !this.cursor) { break; }
        e.preventDefault();
        if (this.multi) { this.toggle(this.cursor); this.anchor = this.cursor; } else { this.set([lbValue(this.cursor)], true); }
        break;
      case "Enter":
        if (!this.cursor) { break; }
        e.preventDefault();
        if (this.checkboxes) { this.toggle(this.cursor); } else if (!this.multi) { this.set([lbValue(this.cursor)], true); }
        break;
      default: {
        if (inFilter) { break; }
        if ((e.key === "a" || e.key === "A") && (e.ctrlKey || e.metaKey) && this.multi) {
          e.preventDefault();
          this.set(rows.map(lbValue), true);
          break;
        }
        // sigil's incremental search: typed letters within 800 ms
        if (e.key && e.key.length === 1 && !e.ctrlKey && !e.altKey && !e.metaKey) {
          const now = Date.now();
          const typed = this.typed = (now - this.typedAt > 800 ? "" : this.typed) + e.key.toLowerCase();
          this.typedAt = now;
          const hit = rows.filter((li) => childText(li, ".ah-listbox-label").toLowerCase().indexOf(typed) === 0)[0];
          if (hit) { e.preventDefault(); this.go(hit, {}); }
        }
      }
    }
  }

  // sigil's filter-items!: hide rows without the text, and empty groups.
  private applyFilter(text: string | null | undefined): void {
    const q = String(text || "").trim().toLowerCase();
    if (!this.remote) {
      this.items().forEach((li) => {
        const hit = !q || childText(li, ".ah-listbox-label").toLowerCase().indexOf(q) >= 0;
        li.style.display = hit ? "" : "none";
      });
    }
    this.groups();
  }

  private groups(): void {
    kids(this.list, ".ah-listbox-group").forEach((g) => {
      let some = false;
      for (let n = g.nextElementSibling as HTMLElement | null; n && !n.matches(".ah-listbox-group");
           n = n.nextElementSibling as HTMLElement | null) {
        if (shown(n)) { some = true; break; }
      }
      g.style.display = some ? "" : "none";
    });
    const some = this.items().some(shown);
    if (this.empty) { this.empty.hidden = some; }
    if (this.cursor && (!this.cursor.isConnected || !shown(this.cursor))) { this.moveCursor(null); }
    this.mark();
  }
}

AH.register("listbox", ListboxController);
