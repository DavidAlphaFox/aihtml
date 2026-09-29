/* Behaviour of the listmenu component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.ts.
   Value contract: data-ah-value (the chosen item's data-key), the hidden
   input and "change" on the root. ah:navigate fires on the root after a
   page change, detail ListmenuNavigate. */
import AH from "../core.ts";
import { byKey, setValue } from "./_lib_nav.ts";

/** Detail of ah:navigate: the item whose page was entered or left, its
 *  label, and the page now shown. */
export interface ListmenuNavigate { id: string | null; label: string; page: string | null; }

function showEl(n: HTMLElement): void { n.style.display = ""; }
function hideEl(n: HTMLElement): void { n.style.display = "none"; }

// Swap the shown page: kind is "none", "fade" or "slide" (dir 1 forward).
function animateSwap(oldEl: HTMLElement | null, newEl: HTMLElement, kind: string, dir: number,
                     done: () => void): void {
  if (!oldEl || oldEl === newEl) {
    showEl(newEl);
    done();
    return;
  }
  if (kind === "none" || typeof newEl.animate !== "function") {
    hideEl(oldEl);
    showEl(newEl);
    done();
    return;
  }
  const dur = 250;
  if (kind === "fade") {
    oldEl.animate([{ opacity: 1 }, { opacity: 0 }], { duration: dur / 2 }).onfinish = () => {
      hideEl(oldEl);
      showEl(newEl);
      newEl.animate([{ opacity: 0 }, { opacity: 1 }], { duration: dur / 2 }).onfinish = done;
    };
    return;
  }
  // slide: the old page leaves to one side while the new one comes in
  const s = dir > 0 ? "-100%" : "100%";
  const e = dir > 0 ? "100%" : "-100%";
  showEl(newEl);
  Object.assign(oldEl.style, { position: "absolute", top: "0", left: "0", width: "100%" });
  oldEl.animate([{ transform: "translateX(0)" }, { transform: "translateX(" + s + ")" }],
                { duration: dur, easing: "ease" });
  newEl.animate([{ transform: "translateX(" + e + ")" }, { transform: "translateX(0)" }],
                { duration: dur, easing: "ease" }).onfinish = () => {
    hideEl(oldEl);
    Object.assign(oldEl.style, { position: "", top: "", left: "", width: "" });
    done();
  };
}

// ------------------------------------------------------------------
// listmenu (sigil listmenu.cljs, listmenu/nav.cljs)
// ------------------------------------------------------------------

const LM_FOCUS = "ah-listmenu-item-focus";

function lmItems(page: Element | null): HTMLElement[] {
  return page ? Array.from(page.querySelectorAll<HTMLElement>(":scope > .ah-listmenu-item")).filter((i) =>
    !i.classList.contains("ah-listmenu-item-disabled") && i.style.display !== "none") : [];
}

class ListmenuController extends AH.Controller {
  /** The data-item-id of each submenu entered, innermost last. */
  #stack: string[] = [];
  #busy = false;

  override setup(): void {
    const el = this.element;
    const s = el.getAttribute("data-ah-stack");
    this.#stack = s ? s.split(",") : [];
    this.#busy = false;
    this.delegate("click", ".ah-listmenu-item", (_e, item) => {
      this.#focus(null);
      this.#activate(item, false);
    });
    this.delegate("click", ".ah-listmenu-back", () => { this.#goBack(false); });
    this.delegate<Event, HTMLInputElement>("input", ".ah-listmenu-filter-input", (e, input) => {
      e.stopPropagation();
      this.#applyFilter(input.value);
    });
    // the filter's own change must not look like a new value
    this.delegate("change", ".ah-listmenu-filter-input", (e) => { e.stopPropagation(); });
    this.listen(el, "keydown", (e) => {
      const t = e.target;
      const inFilter = t instanceof Element && t.classList.contains("ah-listmenu-filter-input");
      const items = lmItems(this.#current());
      const focused = el.querySelector("." + LM_FOCUS);
      const cur = items.findIndex((i) => i === focused);
      switch (e.key) {
        case "ArrowDown":
          e.preventDefault();
          this.#focus(items[Math.min(items.length - 1, cur + 1)]);
          break;
        case "ArrowUp":
          e.preventDefault();
          this.#focus(items[Math.max(0, cur - 1)]);
          break;
        case "Home":
        case "End":
          if (inFilter) { return; }
          e.preventDefault();
          this.#focus(items[e.key === "Home" ? 0 : items.length - 1]);
          break;
        case "Enter":
        case " ":
        case "ArrowRight":
          if (inFilter && e.key !== "Enter") { return; }
          if (cur < 0) { return; }
          e.preventDefault();
          this.#activate(items[cur], true);
          break;
        case "ArrowLeft":
        case "Backspace":
        case "Escape":
          if (inFilter && e.key !== "Escape") { return; }
          if (!this.#stack.length) { return; }
          e.preventDefault();
          this.#goBack(true);
          break;
        default:
          break;
      }
    });
    this.listen(el, "focus", () => {
      if (!el.querySelector("." + LM_FOCUS)) {
        const page = this.#current();
        const sel = page ? page.querySelector<HTMLElement>(":scope > .ah-listmenu-item-selected") : null;
        this.#focus(sel || lmItems(page)[0]);
      }
    });
    this.listen(el, "blur", () => { this.#focus(null); });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(key: string | number | null): void { this.#mark(key); setValue(this.element, key); }
  back(): void { this.#goBack(false); }
  navigate(key: string | number): void {
    const page = this.#current();
    const i = page ? byKey(page, ".ah-listmenu-item[aria-haspopup]", "data-key", key)[0] : undefined;
    if (i) { this.#go(i.getAttribute("data-item-id"), 1, false); }
  }
  filter(text: string | null): void {
    const input = this.element.querySelector<HTMLInputElement>(".ah-listmenu-filter-input");
    if (input) { input.value = text || ""; }
    this.#applyFilter(text);
  }
  currentPage(): string | null | undefined {
    const p = this.#current();
    return p ? p.getAttribute("data-page-id") : undefined;
  }

  #page(id: string): HTMLElement | null {
    return Array.from(this.element.querySelectorAll<HTMLElement>(".ah-listmenu-page")).find((p) =>
      p.getAttribute("data-page-id") === id) || null;
  }

  #itemLabel(itemId: string | null | undefined): string {
    const it = Array.from(this.element.querySelectorAll(".ah-listmenu-item")).find((i) =>
      i.getAttribute("data-item-id") === String(itemId));
    const label = it ? it.querySelector(":scope > .ah-listmenu-item-label") : null;
    return label ? label.textContent || "" : "";
  }

  #focus(item: HTMLElement | null | undefined): void {
    this.element.querySelectorAll("." + LM_FOCUS).forEach((n) => { n.classList.remove(LM_FOCUS); });
    if (item) {
      item.classList.add(LM_FOCUS);
      if (item.scrollIntoView) { item.scrollIntoView({ block: "nearest" }); }
    }
  }

  #mark(key: string | number | null): void {
    const el = this.element;
    el.querySelectorAll(".ah-listmenu-item-selected").forEach((n) => {
      n.classList.remove("ah-listmenu-item-selected");
      n.setAttribute("aria-checked", "false");
    });
    byKey(el, ".ah-listmenu-item:not([aria-haspopup])", "data-key", key).forEach((n) => {
      n.classList.add("ah-listmenu-item-selected");
      n.setAttribute("aria-checked", "true");
    });
  }

  #current(): HTMLElement | null {
    return this.#page(this.#stack.length ? this.#stack[this.#stack.length - 1] : "root");
  }

  #header(): void {
    const el = this.element;
    const root = !this.#stack.length;
    el.querySelectorAll<HTMLElement>(".ah-listmenu-back").forEach((b) => { b.style.display = root ? "none" : ""; });
    const title = root ? "" : this.#itemLabel(this.#stack[this.#stack.length - 1]);
    el.querySelectorAll(".ah-listmenu-title").forEach((t) => { t.textContent = title; });
  }

  #applyFilter(text: string | null): void {
    const t = (text || "").toLowerCase();
    const page = this.#current();
    if (!page) { return; }
    page.querySelectorAll<HTMLElement>(":scope > .ah-listmenu-item").forEach((i) => {
      const l = i.querySelector(":scope > .ah-listmenu-item-label");
      const label = (l ? l.textContent || "" : "").toLowerCase();
      i.style.display = !t || label.indexOf(t) !== -1 ? "" : "none";
    });
  }

  // Enter the page pageId (dir 1) or go back to it (dir -1; null is the
  // root page).
  #go(pageId: string | null, dir: 1 | -1, focus: boolean): void {
    const el = this.element;
    if (this.#busy) { return; }
    const old = this.#current();
    const next = this.#page(pageId === null ? "root" : pageId);
    if (!next) { return; }
    const top = this.#stack[this.#stack.length - 1];
    const label = dir > 0 ? this.#itemLabel(pageId) : this.#itemLabel(top);
    const id = dir > 0 ? pageId : top;
    if (dir > 0) { this.#stack.push(String(pageId)); } else { this.#stack.pop(); }
    this.#header();
    const input = el.querySelector<HTMLInputElement>(".ah-listmenu-filter-input");
    if (input && input.value) {
      input.value = "";
      if (old) { Array.from(old.children).forEach((c) => { if (c instanceof HTMLElement) { showEl(c); } }); }
    }
    this.#busy = true;
    animateSwap(old, next, el.getAttribute("data-ah-animation") || "slide", dir, () => {
      this.#busy = false;
      this.#focus(focus ? lmItems(next)[0] : null);
      this.fire<ListmenuNavigate>("ah:navigate",
                                  { id: id === undefined ? null : id, label: label,
                                    page: next.getAttribute("data-page-id") });
    });
  }

  #goBack(focus: boolean): void {
    if (!this.#stack.length) { return; }
    this.#go(this.#stack.length > 1 ? this.#stack[this.#stack.length - 2] : null, -1, focus);
  }

  #activate(item: HTMLElement, focus: boolean): void {
    const el = this.element;
    if (item.classList.contains("ah-listmenu-item-disabled")) { return; }
    if (item.getAttribute("aria-haspopup")) {
      this.#go(item.getAttribute("data-item-id"), 1, focus);
      return;
    }
    const href = item.getAttribute("data-href");
    if (href) {
      window.location.href = href;
      return;
    }
    const key = item.getAttribute("data-key");
    this.#mark(key);
    if (el.getAttribute("data-ah-value") !== key) { setValue(el, key, "change"); }
  }
}

AH.register("listmenu", ListmenuController);
