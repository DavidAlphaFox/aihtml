/* Behaviour of the combobox (designs/04-components.md). Ported from
 * sigil: form/combobox (+ popup, search). Value contract: data-ah-value,
 * the hidden input and a native "change" on the root. Events on the
 * root: "ah:open", "ah:close" (no detail). An "ah:error" on the text
 * field (its server search failed) ends the loading state. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { ensureId, publish, split, join, kids, highlight } from "./_lib_list.ts";
import "virtual:ah-tpl/combobox_tag";

/** A server-rendered row, read back by read(). */
interface Item {
  el: HTMLElement;
  value: string;
  label: string;
  disabled: boolean;
}

type Match = (text: string, query: string) => boolean;

// search.cljs match-fn
const MATCH: Record<string, Match> = {
  contains_ignore_case: (t, q) => t.toLowerCase().indexOf(q.toLowerCase()) >= 0,
  contains: (t, q) => t.indexOf(q) >= 0,
  starts_with_ignore_case: (t, q) => t.toLowerCase().indexOf(q.toLowerCase()) === 0,
  starts_with: (t, q) => t.indexOf(q) === 0,
  equals_ignore_case: (t, q) => t.toLowerCase() === q.toLowerCase(),
  equals: (t, q) => t === q,
  none: () => true
};

function message(cls: string, text: string): HTMLDivElement {
  const d = document.createElement("div");
  d.className = cls;
  d.textContent = text;
  return d;
}

class ComboboxController extends AH.Controller {
  // parts of the server markup (aihtml_combobox), found in setup
  private input!: HTMLInputElement;
  private popup!: HTMLElement;
  private list: HTMLElement | null = null;
  // settings
  private multi = false;
  private free = false;
  private remote = false;
  private mode = "contains_ignore_case";
  private minLength = 0;
  private emptyText = "No results found";
  private placeholder = "";
  // state
  private items: Item[] = [];
  private labels: Record<string, string> = {};
  private selected: string[] = [];
  private visible: Item[] = [];
  private query = "";
  private isOpen = false;
  private active = -1;
  private loading = false;
  private float: FloatHandle | null = null;

  private readonly onDocDown = (e: MouseEvent): void => {
    // A target that is gone was inside a list re-rendered by this very
    // mousedown (picking in multiple mode).
    const t = e.target as Node | null;
    if (t && t.isConnected !== false && !this.element.contains(t)) { this.close(); }
  };

  override setup(): void {
    const el = this.element;
    ensureId(el, "ah-cb");
    // the text field and the popup are always rendered
    this.input = el.querySelector<HTMLInputElement>("input.ah-combobox-input") as HTMLInputElement;
    this.popup = kids(el, ".ah-combobox-popup")[0] as HTMLElement;
    this.list = kids(this.popup, ".ah-combobox-list")[0] || null;
    this.multi = el.classList.contains("ah-combobox-multiple") || el.classList.contains("ah-combobox-checkboxes");
    this.free = el.classList.contains("ah-combobox-free-text");
    this.remote = el.hasAttribute("data-ah-remote");
    this.mode = el.getAttribute("data-ah-search-mode") || "contains_ignore_case";
    this.minLength = parseInt(el.getAttribute("data-ah-min-length") || "0", 10) || 0;
    this.emptyText = el.getAttribute("data-ah-empty") || "No results found";
    this.placeholder = el.getAttribute("data-ah-placeholder") || "";
    this.items = []; this.labels = {}; this.selected = []; this.visible = [];
    this.query = ""; this.isOpen = false; this.active = -1; this.loading = false;
    if (!this.multi) { this.placeholder = this.input.getAttribute("placeholder") || ""; }
    this.read();
    el.querySelectorAll(".ah-combobox-tag-close").forEach((x) => {
      const v = x.getAttribute("data-value") || "";
      if (!(v in this.labels)) {
        const t = x.parentElement ? kids(x.parentElement, ".ah-combobox-tag-text")[0] : null;
        this.labels[v] = t ? t.textContent || "" : "";
      }
    });
    this.selected = split(el.getAttribute("data-ah-value") || "", !this.multi);
    if (!this.multi && this.selected.length && !(this.selected[0] in this.labels)) {
      this.labels[this.selected[0]] = this.input.value;
    }
    this.input.setAttribute("data-combobox", el.id);
    if (this.list && this.list.id) { this.input.setAttribute("aria-controls", this.list.id); }
    else { this.input.removeAttribute("aria-controls"); }

    const input = this.input;
    this.listen(input, "focus", () => { el.classList.add("ah-combobox-focused"); });
    this.listen(input, "blur", () => {
      el.classList.remove("ah-combobox-focused");
      // popup.cljs: close a moment later, after a click on an item
      setTimeout(() => {
        if (document.activeElement !== input) {
          this.close();
          this.settle();
        }
      }, 150);
    });
    this.listen(input, "input", () => {
      this.query = String(input.value);
      if (this.query.length < this.minLength) { this.close(); return; }
      this.loading = this.remote;     // until set_items answers (itemsLoaded)
      this.open();
    });
    this.listen(input, "click", (e) => {
      e.preventDefault();
      if (this.isOpen) { this.close(); } else { this.open(); }
    });
    this.listen(input, "keydown", (e) => { this.key(e); });
    // the text field is internal: only the root reports changes
    this.listen(input, "change", (e) => { e.stopPropagation(); });
    this.listen(input, "ah:error", () => {
      if (this.loading) { this.loading = false; if (this.isOpen) { this.applyFilter(); this.position(); } }
    });
    this.delegate("mousedown", ".ah-combobox-arrow, .ah-combobox-tag-close", (e) => {
      e.preventDefault();
    });
    this.delegate("click", ".ah-combobox-arrow", (e) => {
      e.preventDefault();
      input.focus();
      if (this.isOpen) { this.close(); } else { this.open(); }
    });
    this.delegate("click", ".ah-combobox-tag-close", (e, x) => {
      e.preventDefault();
      e.stopPropagation();
      if (this.blocked()) { return; }
      const i = this.selected.indexOf(x.getAttribute("data-value") || "");
      if (i >= 0) { this.selected.splice(i, 1); }
      this.sync(true);
      if (this.isOpen) { this.mark(); this.position(); }
    });
    // popup.cljs selects on mousedown, keeping the focus in the field.
    this.listen(this.popup, "mousedown", (e) => { e.preventDefault(); });
    this.delegate("mousedown", ".ah-combobox-item", (_e, li) => {
      this.pick(this.visible.filter((it) => it.el === li)[0]);
    }, this.popup);
    this.delegate("mouseover", ".ah-combobox-item", (_e, li) => {
      const i = this.visible.map((it) => it.el).indexOf(li);
      if (i !== this.active) { this.setActive(i); }
    }, this.popup);
  }

  override teardown(): void {
    if (this.float) { this.float.stop(); this.float = null; }
    document.removeEventListener("mousedown", this.onDocDown);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  /** Called by aihtml_combobox:set_items/3,4 after it morphed the
   *  server-rendered items into the list. */
  itemsLoaded(): void {
    this.loading = false;
    this.read();
    if (this.isOpen || document.activeElement === this.input) {
      this.open();
    } else {
      this.applyFilter();
    }
  }
  /** A value or a list (multiple); no change event (the server set it). */
  setValue(v: string | readonly string[] | null): void { this.assign(v, false); }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  clear(): void { this.assign([], true); }

  open(): void {
    if (this.blocked()) { return; }
    this.applyFilter();
    if (this.isOpen) { this.position(); return; }
    this.isOpen = true;
    this.popup.classList.add("ah-combobox-popup-open");
    this.position();
    this.element.classList.add("ah-combobox-open");
    this.input.setAttribute("aria-expanded", "true");
    document.addEventListener("mousedown", this.onDocDown);
    this.fire("ah:open");
  }

  close(): void {
    if (!this.isOpen) { return; }
    this.isOpen = false;
    this.popup.classList.remove("ah-combobox-popup-open");
    if (this.float) { this.float.stop(); this.float = null; }
    this.element.classList.remove("ah-combobox-open");
    this.input.setAttribute("aria-expanded", "false");
    this.input.removeAttribute("aria-activedescendant");
    this.active = -1;
    document.removeEventListener("mousedown", this.onDocDown);
    this.fire("ah:close");
  }

  private blocked(): boolean { return this.element.classList.contains("ah-combobox-disabled"); }

  // The items are the server-rendered <li>s (first render, or morphed in
  // by aihtml_combobox:set_items); this reads them back.
  private read(): void {
    this.items = kids(this.list, ".ah-combobox-item").map((li) => {
      const it: Item = {
        el: li,
        value: li.getAttribute("data-value") || "",
        label: li.getAttribute("data-label") || "",
        disabled: li.getAttribute("aria-disabled") === "true"
      };
      this.labels[it.value] = it.label;
      return it;
    });
  }

  private label(v: string): string {
    return Object.prototype.hasOwnProperty.call(this.labels, v) ? this.labels[v] : v;
  }

  // Selected state of the rows (after picks in multiple mode, or new rows).
  private mark(): void {
    this.items.forEach((it) => {
      const sel = this.selected.indexOf(it.value) >= 0;
      it.el.classList.toggle("ah-combobox-item-selected", sel);
      it.el.setAttribute("aria-selected", String(sel));
      kids(it.el, ".ah-combobox-checkbox").forEach((box) => {
        box.classList.toggle("ah-combobox-checkbox-checked", sel);
        const icon = kids(box, ".ah-combobox-checkbox-icon");
        if (sel && !icon.length) {
          const span = document.createElement("span");
          span.className = "ah-combobox-checkbox-icon";
          span.textContent = "✓";
          box.appendChild(span);
        } else if (!sel) {
          icon.forEach((i) => { i.remove(); });
        }
      });
    });
  }

  // popup.cljs update-query!: show the matching rows (all of them for
  // server results), highlight the query, hide empty groups, and show the
  // empty or loading message.
  private applyFilter(): void {
    const match: Match | null = (this.remote || !this.query || this.mode === "none") ? null
      : (MATCH[this.mode] || MATCH.contains_ignore_case);
    this.visible = [];
    this.items.forEach((it) => {
      const show = !match || match(it.label, this.query);
      it.el.style.display = show ? "" : "none";
      if (show) { this.visible.push(it); }
      highlight(it.el.querySelector(".ah-combobox-item-label"), it.label, this.query);
    });
    kids(this.list, ".ah-combobox-group-header").forEach((h) => {
      let some = false;
      for (let n = h.nextElementSibling as HTMLElement | null; n && !n.matches(".ah-combobox-group-header");
           n = n.nextElementSibling as HTMLElement | null) {
        if (n.style.display !== "none") { some = true; break; }
      }
      h.style.display = some ? "" : "none";
    });
    this.mark();
    kids(this.popup, ".ah-combobox-empty, .ah-combobox-loading").forEach((m) => { m.remove(); });
    if (this.loading) {
      this.popup.appendChild(message("ah-combobox-loading", "Loading…"));
    } else if (!this.visible.length) {
      this.popup.appendChild(message("ah-combobox-empty", this.emptyText));
    }
    if (this.list) { this.list.style.display = (!this.loading && this.visible.length > 0) ? "" : "none"; }
    this.setActive(-1);
  }

  private setActive(idx: number): void {
    this.active = idx;
    this.items.forEach((it) => { it.el.classList.remove("ah-combobox-item-active"); });
    const item = idx >= 0 && this.visible[idx] ? this.visible[idx].el : null;
    if (!item) {
      this.active = -1;
      this.input.removeAttribute("aria-activedescendant");
      return;
    }
    item.classList.add("ah-combobox-item-active");
    if (item.id) { this.input.setAttribute("aria-activedescendant", item.id); }
    else { this.input.removeAttribute("aria-activedescendant"); }
    const p = this.popup;               // popup.cljs scroll-item-into-view!
    if (item.offsetTop < p.scrollTop) { p.scrollTop = item.offsetTop; }
    if (item.offsetTop + item.offsetHeight > p.scrollTop + p.clientHeight) {
      p.scrollTop = item.offsetTop + item.offsetHeight - p.clientHeight;
    }
  }

  private moveBy(dir: number): void {
    const n = this.visible.length;
    if (!n) { return; }
    let i = this.active;
    for (let k = 0; k < n; k++) {
      i = dir > 0 ? (i < n - 1 ? i + 1 : 0) : (i > 0 ? i - 1 : n - 1);
      if (!this.visible[i].disabled) { this.setActive(i); return; }
    }
  }

  // AH.float: fixed at the field, at least as wide, flipped above when
  // there is no room; update() after the list or the tags change size.
  private position(): void {
    if (!this.isOpen) { return; }
    if (this.float) {
      this.float.update();
    } else {
      this.float = AH.float(this.popup, this.element, { matchWidth: true });
    }
  }

  // Tags added in the browser use the server's markup:
  // templates/combobox_tag.mustache
  private tags(): void {
    const input = this.input;
    const box = input.parentElement;
    if (box) {
      Array.from(box.children).forEach((c) => {
        if (c !== input && c.matches(".ah-combobox-tag")) { c.remove(); }
      });
    }
    this.selected.forEach((v) => {
      input.insertAdjacentHTML("beforebegin", AH.tpl.combobox_tag({ value: v, label: this.label(v) }));
    });
    input.setAttribute("placeholder", this.selected.length ? "" : this.placeholder);
  }

  private sync(fire: boolean): void {
    if (this.multi) {
      this.tags();
    } else {
      this.input.value = this.selected.length ? this.label(this.selected[0]) : "";
    }
    publish(this.element, join(this.selected, !this.multi), fire);
  }

  // popup.cljs select-single-item! / toggle-multi-item!
  private pick(it: Item | undefined): void {
    if (!it || it.disabled) { return; }
    if (this.multi) {
      const i = this.selected.indexOf(it.value);
      if (i >= 0) { this.selected.splice(i, 1); } else { this.selected.push(it.value); }
      this.sync(true);
      this.mark();
      this.position();
    } else {
      this.selected = [it.value];
      this.query = "";
      this.sync(true);
      this.close();
    }
  }

  // Leaving the field: free text becomes the value, otherwise the text
  // goes back to the selected item's label (an emptied field clears it).
  private settle(): void {
    const input = this.input;
    if (this.multi) {
      if (!this.free) { input.value = ""; this.query = ""; return; }
      const t = String(input.value).trim();
      if (t && this.selected.indexOf(t) < 0) { this.selected.push(t); this.labels[t] = t; }
      input.value = "";
      this.query = "";
      this.sync(true);
      return;
    }
    const text = String(input.value);
    const current = this.selected.length ? this.label(this.selected[0]) : "";
    this.query = "";
    if (text === current) { return; }
    if (text === "") {
      this.selected = [];
    } else if (this.free) {
      const exact = this.items.filter((it) => it.label === text)[0];
      this.selected = [exact ? exact.value : text];
      if (!exact) { this.labels[text] = text; }
    }
    this.sync(true);
  }

  private key(e: KeyboardEvent): void {
    if (this.blocked()) { return; }
    switch (e.key) {
      case "ArrowDown":
        e.preventDefault();
        if (!this.isOpen || e.altKey) { this.open(); } else { this.moveBy(1); }
        break;
      case "ArrowUp":
        e.preventDefault();
        if (e.altKey) { this.close(); } else if (this.isOpen) { this.moveBy(-1); }
        break;
      case "Enter":
        e.preventDefault();
        if (!this.isOpen) { this.open(); return; }
        if (this.active >= 0) {
          this.pick(this.visible[this.active]);
        } else if (this.free) {
          this.settle();
          this.close();
        } else {
          const enabled = this.visible.filter((it) => !it.disabled);
          if (enabled.length === 1) { this.pick(enabled[0]); }
        }
        break;
      case "Escape":
        if (this.isOpen) {
          e.preventDefault();
          this.close();
        } else if (!this.multi) {
          this.input.value = this.selected.length ? this.label(this.selected[0]) : "";
          this.query = "";
        }
        break;
      case "Tab":
        if (this.isOpen && this.active >= 0 && !this.multi) {
          this.pick(this.visible[this.active]);
        } else {
          this.close();
        }
        break;
      case "Backspace":
        if (this.multi && this.input.value === "" && this.selected.length) {
          this.selected.pop();
          this.sync(true);
          if (this.isOpen) { this.mark(); this.position(); }
        }
        break;
      default:
        break;
    }
  }

  private assign(v: unknown, fire: boolean): void {
    const vals = v == null || v === "" ? [] : split(v, !this.multi);
    this.selected = vals.slice(0, this.multi ? vals.length : 1);
    this.query = "";
    this.sync(fire);
    if (this.isOpen) { this.applyFilter(); } else { this.mark(); }
  }
}

AH.register("combobox", ComboboxController);
