/* Behaviour of the navbar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.ts.
   Value contract: data-ah-value (the selected item's data-key), the
   hidden input and "change" on the root. */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { byKey, hover, setValue } from "./_lib_nav.ts";

// ------------------------------------------------------------------
// navbar (sigil navbar.cljs, navbar/popup.cljs)
// ------------------------------------------------------------------

function closestTo(t: EventTarget | null, sel: string): HTMLElement | null {
  return t instanceof Element ? t.closest<HTMLElement>(sel) : null;
}

/** Mark the item with key selected in scope (the bar or its popup). */
function mark(scope: Element, key: unknown): void {
  const bar = scope.classList.contains("ah-navbar");
  scope.querySelectorAll(".ah-navbar-item").forEach((it) => {
    const on = it.getAttribute("data-key") === String(key);
    it.classList.toggle("ah-navbar-item-selected", on);
    it.setAttribute("aria-selected", String(on));
    if (bar) { it.setAttribute("tabindex", on ? "0" : "-1"); }
  });
}

class NavbarController extends AH.Controller {
  #popup: HTMLElement | null = null;
  #popupFloat: FloatHandle | null = null;

  override setup(): void {
    const el = this.element;
    this.#popup = null;
    this.delegate("click", ".ah-navbar-item", (e, item) => { this.#select(item, e); });
    hover(this, ".ah-navbar-item",
          (item) => { item.classList.add("ah-navbar-item-hover"); },
          (item) => { item.classList.remove("ah-navbar-item-hover"); });
    // tabs pattern: arrows move focus, Enter / Space select
    this.delegate("keydown", ".ah-navbar-item", (e, item) => {
      const items = Array.from(el.querySelectorAll<HTMLElement>(":scope > .ah-navbar-item")).filter((i) =>
        !i.classList.contains("ah-navbar-item-disabled"));
      const i = items.indexOf(item);
      const n = items.length;
      let to: number;
      switch (e.key) {
        case "ArrowRight": case "ArrowDown": to = (i + 1) % n; break;
        case "ArrowLeft": case "ArrowUp": to = (i - 1 + n) % n; break;
        case "Home": to = 0; break;
        case "End": to = n - 1; break;
        case "Enter": case " ":
          e.preventDefault();
          item.click();
          return;
        default: return;
      }
      e.preventDefault();
      items.forEach((x) => { x.setAttribute("tabindex", "-1"); });
      items[to].setAttribute("tabindex", "0");
      items[to].focus();
    });
    const toggle = (): HTMLElement | null => {
      if (this.#popup) { this.#popupClose(); return null; }
      return this.#popupOpen();
    };
    this.delegate("click", ".ah-navbar-header", () => { toggle(); });
    this.delegate("keydown", ".ah-navbar-header", (e) => {
      if (e.key === "Enter" || e.key === " " || e.key === "ArrowDown") {
        e.preventDefault();
        const first = this.#popup && e.key === "ArrowDown" ? null : toggle();
        if (first) { first.focus(); }
      } else if (e.key === "Escape") {
        this.#popupClose();
      }
    });
    this.listen(document, "mousedown", (e) => {
      const p = this.#popup;
      const t = e.target;
      if (p && !(t instanceof Node && p.contains(t)) && !closestTo(t, ".ah-navbar-header")) {
        this.#popupClose();
      }
    });
    const minW = parseInt(el.getAttribute("data-ah-minimize-width") || "", 10);
    if (minW && el.getAttribute("data-ah-minimized") !== "static") {
      const check = (): void => {
        const small = window.innerWidth <= minW;
        el.classList.toggle("ah-navbar-minimized", small);
        if (!small) { this.#popupClose(); }
      };
      this.listen(window, "resize", check);
      check();
    }
  }

  override teardown(): void { this.#popupClose(); }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(key: string | number | null): void { mark(this.element, key); setValue(this.element, key); }
  select(key: string | number): void {
    const i = byKey(this.element, ".ah-navbar-item", "data-key", key)[0];
    if (i) { this.#select(i, null); }
  }
  minimize(): void { this.element.classList.add("ah-navbar-minimized"); }
  restore(): void { this.element.classList.remove("ah-navbar-minimized"); this.#popupClose(); }

  #select(item: HTMLElement, e: Event | null): void {
    const el = this.element;
    if (item.classList.contains("ah-navbar-item-disabled") || el.getAttribute("data-ah-selection") === "false") {
      if (e && !item.getAttribute("href")) { e.preventDefault(); }
      return;
    }
    const key = item.getAttribute("data-key");
    mark(el, key);
    if (!item.getAttribute("href")) {
      if (e) { e.preventDefault(); }
      if (el.getAttribute("data-ah-value") !== key) { setValue(el, key, "change"); }
    }
  }

  #headers(): NodeListOf<Element> { return this.element.querySelectorAll(".ah-navbar-header"); }

  #popupClose(): void {
    const p = this.#popup;
    if (p) {
      if (this.#popupFloat) { this.#popupFloat.stop(); this.#popupFloat = null; }
      p.remove();
      this.#popup = null;
      this.#headers().forEach((h) => { h.setAttribute("aria-expanded", "false"); });
    }
  }

  // Open the popup listing the items; returns the item to focus.
  #popupOpen(): HTMLElement | null {
    const el = this.element;
    const p = document.createElement("div");
    p.className = "ah-navbar-popup";
    p.setAttribute("role", "listbox");
    el.querySelectorAll(":scope > .ah-navbar-item").forEach((it) => {
      const c = it.cloneNode(true) as Element;      // a clone of an element is one
      c.removeAttribute("id");
      c.removeAttribute("style");
      c.setAttribute("role", "option");
      c.setAttribute("tabindex", "0");
      c.querySelectorAll("[id]").forEach((n) => { n.removeAttribute("id"); });
      p.appendChild(c);
    });
    p.style.width = el.offsetWidth + "px";
    p.style.display = "block";
    document.body.appendChild(p);
    this.#popup = p;
    this.#popupFloat = AH.float(p, el, { placement: "bottom", offset: 0, matchWidth: true });
    this.#headers().forEach((h) => { h.setAttribute("aria-expanded", "true"); });
    // the popup is removed on close, its listeners too
    p.addEventListener("click", (e) => {
      const item = closestTo(e.target, ".ah-navbar-item");
      if (!item || !p.contains(item)) { return; }
      const orig = byKey(el, ".ah-navbar-item", "data-key", item.getAttribute("data-key"))[0];
      if (orig) { this.#select(orig, e); }
      this.#popupClose();
    });
    p.addEventListener("keydown", (e) => {
      const item = closestTo(e.target, ".ah-navbar-item");
      if (!item || !p.contains(item)) { return; }
      const items = Array.from(p.querySelectorAll<HTMLElement>(":scope > .ah-navbar-item"));
      const i = items.indexOf(item);
      const head = el.querySelector<HTMLElement>(".ah-navbar-header");
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        e.preventDefault();
        items[(i + (e.key === "ArrowDown" ? 1 : -1) + items.length) % items.length].focus();
      } else if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        item.click();
        if (head) { head.focus(); }
      } else if (e.key === "Escape") {
        this.#popupClose();
        if (head) { head.focus(); }
      }
    });
    return p.querySelector<HTMLElement>(":scope > .ah-navbar-item-selected") ||
      p.querySelector<HTMLElement>(":scope > .ah-navbar-item");
  }
}

AH.register("navbar", NavbarController);
