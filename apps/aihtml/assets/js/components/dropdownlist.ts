/* Behaviour of dropdownlist (designs/04-components.md). Ported from sigil
   (sigil.components.form.{dropdownlist, listbox}). Value contract:
   data-ah-value, the hidden input and a native "change" on the root,
   whose detail is {value, label} (DropdownlistChange; label null when
   cleared). Events on the root: "ah:open", "ah:close" (no detail). */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { ensureId, kids } from "./_lib_list.ts";

/** Detail of the root's change event. */
export interface DropdownlistChange {
  value: string;
  label: string | null;
}

function trim(s: string | null | undefined): string { return String(s === null || s === undefined ? "" : s).trim(); }

function isOff(item: Element): boolean { return item.classList.contains("ah-listbox-item-disabled"); }
function shownItem(item: HTMLElement): boolean { return item.style.display !== "none"; }
function isSelected(item: Element): boolean { return item.classList.contains("ah-listbox-item-selected"); }

function label(item: Element): string {
  const l = item.querySelector(".ah-listbox-label");
  return trim((l || item).textContent);
}

// ------------------------------------------------------------------
// dropdownlist
// ------------------------------------------------------------------
//
// Keyboard (combobox pattern, extending sigil's Enter/Space/Escape/F4/
// Alt+arrows with listbox navigation):
//   closed: ArrowDown/ArrowUp/Enter/Space/F4/Alt+Arrow open, a letter
//           selects the next item starting with it
//   open:   ArrowUp/Down/Home/End/PageUp/PageDown move, Enter/Space
//           select, Escape/Alt+Arrow/F4 close, Tab closes, letters
//           type-ahead (or filter, when filterable)

class DropdownlistController extends AH.Controller {
  private isOpen = false;
  private search = "";
  private searchAt = 0;
  private float: FloatHandle | null = null;

  override setup(): void {
    const el = this.element;
    this.isOpen = false; this.search = ""; this.searchAt = 0;
    const id = ensureId(el, "ah-dd");
    el.querySelectorAll(".ah-listbox-list").forEach((l) => { l.id = id + "-list"; });
    el.setAttribute("aria-controls", id + "-list");
    this.items().forEach((it) => {
      it.id = id + "-opt-" + it.getAttribute("data-idx");
    });

    this.delegate("click", ".ah-dropdownlist-input-area", () => {
      if (this.isOpen) { this.shut(true); } else { this.open(); }
    });
    this.delegate("click", ".ah-listbox-item", (e, item) => {
      e.stopPropagation();
      if (isOff(item)) { return; }
      this.select(item);
      this.shut(true);
    });
    // Keep focus on the combobox while the pointer is in the popup.
    this.delegate("mousedown", ".ah-dropdownlist-popup", (e) => {
      const t = e.target as Element;
      if (!t.classList.contains("ah-listbox-filter-input")) { e.preventDefault(); }
    });
    this.delegate("mousemove", ".ah-listbox-item", (_e, item) => {
      if (!isOff(item) && this.activeItem() !== item) { this.setActive(item); }
    });
    this.listen(el, "keydown", (e) => { this.keydown(e); });
    this.delegate<Event, HTMLInputElement>("input", ".ah-listbox-filter-input", (e, input) => {
      e.stopPropagation();           // not the component's own input event
      this.applyFilter(input.value);
    });
    this.delegate("change", ".ah-listbox-filter-input", (e) => { e.stopPropagation(); });
    this.listen(el, "focusin", () => { el.classList.add("ah-dropdownlist-focused"); });
    this.listen(el, "focusout", (e) => {
      const to = e.relatedTarget as Node | null;
      if (!to || !el.contains(to)) {
        el.classList.remove("ah-dropdownlist-focused");
        this.shut(false);
      }
    });
    this.listen(document, "mousedown", (e) => {
      if (this.isOpen && !el.contains(e.target as Node | null)) { this.shut(false); }
    });
  }

  override teardown(): void { this.shut(false); }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void {
    const el = this.element;
    if (this.isOpen || this.blocked()) { return; }
    const popup = this.popup();
    if (!popup) { return; }
    this.isOpen = true;
    el.classList.add("ah-dropdownlist-open", "ah-dropdownlist-state-selected");
    el.setAttribute("aria-expanded", "true");
    popup.classList.add("ah-dropdownlist-popup-open");
    // Shared positioning: fixed, at least the root's width, flips above
    // when there is no room below, follows scroll and resize.
    this.float = AH.float(popup, el, { placement: "bottom", align: "start", offset: 4,
                                       matchWidth: true });
    this.placement();
    const sel = this.items().filter(isSelected)[0];
    this.setActive(sel || this.enabledItems()[0]);
    const filter = this.filterInput();
    if (filter) { filter.focus(); }
    this.fire("ah:open");
  }
  close(): void { this.shut(false); }
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  /** setValue(value[, silent]): select the item with that value
   *  ("" or null clears); fires change unless silent. */
  setValue(value: unknown, silent?: boolean): void {
    const v = value === null || value === undefined ? "" : String(value);
    const item = this.items().filter((it) => it.getAttribute("data-value") === v)[0] || null;
    this.select(item, silent);
  }
  disable(): void {
    this.shut(false);
    this.element.classList.add("ah-dropdownlist-disabled");
    this.element.setAttribute("aria-disabled", "true");
    this.element.setAttribute("tabindex", "-1");
  }
  enable(): void {
    this.element.classList.remove("ah-dropdownlist-disabled");
    this.element.removeAttribute("aria-disabled");
    this.element.setAttribute("tabindex", "0");
  }

  private items(): HTMLElement[] { return Array.from(this.element.querySelectorAll<HTMLElement>(".ah-listbox-item")); }
  private enabledItems(): HTMLElement[] { return this.items().filter((it) => shownItem(it) && !isOff(it)); }
  private popup(): HTMLElement | null { return kids(this.element, ".ah-dropdownlist-popup")[0] || null; }
  private filterInput(): HTMLInputElement | null {
    return this.element.querySelector<HTMLInputElement>(".ah-listbox-filter-input");
  }
  private blocked(): boolean { return this.element.classList.contains("ah-dropdownlist-disabled"); }
  private activeItem(): HTMLElement | null { return this.element.querySelector<HTMLElement>(".ah-listbox-item-focused"); }

  private setActive(item: HTMLElement | null | undefined): void {
    this.items().forEach((it) => { it.classList.remove("ah-listbox-item-focused"); });
    const filter = this.filterInput();
    if (!item) {
      this.element.removeAttribute("aria-activedescendant");
      if (filter) { filter.removeAttribute("aria-activedescendant"); }
      return;
    }
    item.classList.add("ah-listbox-item-focused");
    this.element.setAttribute("aria-activedescendant", item.id);
    if (filter) { filter.setAttribute("aria-activedescendant", item.id); }
    if (typeof item.scrollIntoView === "function") { item.scrollIntoView({ block: "nearest" }); }
  }

  // sigil's -above / -below classes, from the side AH.float chose.
  private placement(): void {
    const popup = this.popup();
    if (!popup) { return; }
    const above = popup.getAttribute("data-ah-placement") === "top";
    popup.classList.toggle("ah-dropdownlist-popup-above", above);
    popup.classList.toggle("ah-dropdownlist-popup-below", !above);
  }

  private shut(refocus: boolean): void {
    const el = this.element;
    if (!this.isOpen) { return; }
    this.isOpen = false;
    el.classList.remove("ah-dropdownlist-open", "ah-dropdownlist-state-selected");
    el.setAttribute("aria-expanded", "false");
    if (this.float) { this.float.stop(); this.float = null; }
    const popup = this.popup();
    if (popup) {
      popup.classList.remove("ah-dropdownlist-popup-open", "ah-dropdownlist-popup-above",
                             "ah-dropdownlist-popup-below");
    }
    this.setActive(null);
    const filter = this.filterInput();
    if (filter && filter.value) {
      filter.value = "";
      this.applyFilter("");
    }
    if (refocus && el.contains(document.activeElement) && document.activeElement !== el) {
      el.focus();
    }
    this.fire("ah:close");
  }

  // Select an item (null clears). Fires change when the value changes.
  private select(item: HTMLElement | null, silent?: boolean): void {
    const el = this.element;
    const value = item ? item.getAttribute("data-value") || "" : "";
    const old = el.getAttribute("data-ah-value") || "";
    this.items().forEach((it) => {
      it.classList.remove("ah-listbox-item-selected");
      it.setAttribute("aria-selected", "false");
    });
    el.querySelectorAll(".ah-dropdownlist-content").forEach((content) => {
      if (item) {
        content.textContent = label(item);
        content.classList.remove("ah-dropdownlist-content-placeholder");
      } else {
        content.textContent = el.getAttribute("data-ah-placeholder") || "";
        content.classList.add("ah-dropdownlist-content-placeholder");
      }
    });
    if (item) {
      item.classList.add("ah-listbox-item-selected");
      item.setAttribute("aria-selected", "true");
    }
    el.setAttribute("data-ah-value", value);
    el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = value; });
    if (!silent && value !== old) {
      this.fire<DropdownlistChange>("change", { value: value, label: item ? label(item) : null });
    }
  }

  private applyFilter(text: string): void {
    const q = trim(text).toLowerCase();
    this.items().forEach((it) => {
      it.style.display = !q || label(it).toLowerCase().indexOf(q) >= 0 ? "" : "none";
    });
    this.element.querySelectorAll<HTMLElement>(".ah-listbox-group").forEach((g) => {
      let some = false;
      for (let n = g.nextElementSibling as HTMLElement | null; n && !n.matches(".ah-listbox-group");
           n = n.nextElementSibling as HTMLElement | null) {
        if (n.style.display !== "none") { some = true; break; }
      }
      g.style.display = some ? "" : "none";
    });
    this.setActive(this.enabledItems()[0]);
    if (this.float) { this.float.update(); this.placement(); }
  }

  // Type-ahead: letters typed within 800 ms form one prefix (sigil's
  // incremental search). Returns the matching item after the current one.
  private typeahead(key: string): HTMLElement | null {
    const now = Date.now();
    this.search = now - this.searchAt > 800 ? key : this.search + key;
    this.searchAt = now;
    const q = this.search.toLowerCase();
    const items = this.enabledItems();
    const cur = this.isOpen ? this.activeItem() : this.items().filter(isSelected)[0];
    const at = cur ? items.indexOf(cur) : -1;
    const start = this.search.length === 1 ? at + 1 : Math.max(at, 0);
    for (let i = 0; i < items.length; i++) {
      const it = items[(start + i) % items.length];
      if (label(it).toLowerCase().indexOf(q) === 0) { return it; }
    }
    return null;
  }

  private keydown(e: KeyboardEvent): void {
    if (this.blocked()) { return; }
    const key = e.key || "";
    const inFilter = (e.target as Element).classList.contains("ah-listbox-filter-input");
    const toggle = key === "F4" || (e.altKey && (key === "ArrowDown" || key === "ArrowUp"));
    if (!this.isOpen) {
      if (toggle || key === "ArrowDown" || key === "ArrowUp" || key === "Enter" || key === " ") {
        e.preventDefault();
        this.open();
      } else if (key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
        const hit = this.typeahead(key);
        if (hit) { this.select(hit); }
      }
      return;
    }
    const items = this.enabledItems();
    const active = this.activeItem();
    const idx = active ? items.indexOf(active) : -1;
    const move = (i: number): void => {
      e.preventDefault();
      if (items.length) { this.setActive(items[Math.max(0, Math.min(items.length - 1, i))]); }
    };
    if (toggle || key === "Escape") {
      e.preventDefault();
      this.shut(true);
    } else if (key === "ArrowDown") {
      move(idx + 1);
    } else if (key === "ArrowUp") {
      move(idx < 0 ? items.length - 1 : idx - 1);
    } else if (key === "Home" && !inFilter) {
      move(0);
    } else if (key === "End" && !inFilter) {
      move(items.length - 1);
    } else if (key === "PageDown") {
      move(idx + 10);
    } else if (key === "PageUp") {
      move(idx - 10);
    } else if (key === "Enter" || (key === " " && !inFilter)) {
      e.preventDefault();
      if (idx >= 0) { this.select(items[idx]); }
      this.shut(true);
    } else if (key === "Tab") {
      this.shut(false);
    } else if (!inFilter && key.length === 1 && !e.ctrlKey && !e.metaKey && !e.altKey) {
      const hit = this.typeahead(key);
      if (hit) { this.setActive(hit); }
    }
  }
}

AH.register("dropdownlist", DropdownlistController);
