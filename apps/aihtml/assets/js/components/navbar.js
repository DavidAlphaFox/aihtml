/* Behaviour of the navbar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js.
   Value contract: data-ah-value (the selected item's data-key), the
   hidden input and "change" on the root. */
import AH from "../core.js";
import "./_lib_nav.js";

const N = AH.lib.nav;
const setValue = N.setValue;
const byKey = N.byKey;

// ------------------------------------------------------------------
// navbar (sigil navbar.cljs, navbar/popup.cljs)
// ------------------------------------------------------------------

function navbarMark(scope, key) {
  const bar = scope.classList.contains("ah-navbar");
  scope.querySelectorAll(".ah-navbar-item").forEach(function (it) {
    const on = it.getAttribute("data-key") === String(key);
    it.classList.toggle("ah-navbar-item-selected", on);
    it.setAttribute("aria-selected", String(on));
    if (bar) { it.setAttribute("tabindex", on ? "0" : "-1"); }
  });
}

function navbarSelect(el, item, e) {
  if (item.classList.contains("ah-navbar-item-disabled") || el.getAttribute("data-ah-selection") === "false") {
    if (e && !item.getAttribute("href")) { e.preventDefault(); }
    return;
  }
  const key = item.getAttribute("data-key");
  navbarMark(el, key);
  if (!item.getAttribute("href")) {
    if (e) { e.preventDefault(); }
    if (el.getAttribute("data-ah-value") !== key) { setValue(el, key, "change"); }
  }
}

function headers(el) { return el.querySelectorAll(".ah-navbar-header"); }

AH.register("navbar", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.popup = null;
    this.delegate("click", ".ah-navbar-item", function (e, item) { navbarSelect(el, item, e); });
    N.hover(this, ".ah-navbar-item",
            function (item) { item.classList.add("ah-navbar-item-hover"); },
            function (item) { item.classList.remove("ah-navbar-item-hover"); });
    // tabs pattern: arrows move focus, Enter / Space select
    this.delegate("keydown", ".ah-navbar-item", function (e, item) {
      const items = Array.from(el.querySelectorAll(":scope > .ah-navbar-item")).filter(function (i) {
        return !i.classList.contains("ah-navbar-item-disabled");
      });
      const i = items.indexOf(item);
      const n = items.length;
      let to = null;
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
      items.forEach(function (x) { x.setAttribute("tabindex", "-1"); });
      items[to].setAttribute("tabindex", "0");
      items[to].focus();
    });
    const toggle = function () {
      if (self.popup) { self.popupClose(); return null; }
      return self.popupOpen();
    };
    this.delegate("click", ".ah-navbar-header", function () { toggle(); });
    this.delegate("keydown", ".ah-navbar-header", function (e) {
      if (e.key === "Enter" || e.key === " " || e.key === "ArrowDown") {
        e.preventDefault();
        const first = self.popup && e.key === "ArrowDown" ? null : toggle();
        if (first) { first.focus(); }
      } else if (e.key === "Escape") {
        self.popupClose();
      }
    });
    this.listen(document, "mousedown", function (e) {
      const p = self.popup;
      const t = e.target;
      if (p && !p.contains(t) && !(t.closest && t.closest(".ah-navbar-header"))) {
        self.popupClose();
      }
    });
    const minW = parseInt(el.getAttribute("data-ah-minimize-width"), 10);
    if (minW && el.getAttribute("data-ah-minimized") !== "static") {
      const check = function () {
        const small = window.innerWidth <= minW;
        el.classList.toggle("ah-navbar-minimized", small);
        if (!small) { self.popupClose(); }
      };
      this.listen(window, "resize", check);
      check();
    }
  }

  teardown() { this.popupClose(); }

  popupClose() {
    const p = this.popup;
    if (p) {
      if (this.popupFloat) { this.popupFloat.stop(); this.popupFloat = null; }
      p.remove();
      this.popup = null;
      headers(this.element).forEach(function (h) { h.setAttribute("aria-expanded", "false"); });
    }
  }

  popupOpen() {
    const el = this.element;
    const self = this;
    const p = document.createElement("div");
    p.className = "ah-navbar-popup";
    p.setAttribute("role", "listbox");
    el.querySelectorAll(":scope > .ah-navbar-item").forEach(function (it) {
      const c = it.cloneNode(true);
      c.removeAttribute("id");
      c.removeAttribute("style");
      c.setAttribute("role", "option");
      c.setAttribute("tabindex", "0");
      c.querySelectorAll("[id]").forEach(function (n) { n.removeAttribute("id"); });
      p.appendChild(c);
    });
    p.style.width = el.offsetWidth + "px";
    p.style.display = "block";
    document.body.appendChild(p);
    this.popup = p;
    this.popupFloat = AH.float(p, el, { placement: "bottom", offset: 0, matchWidth: true });
    headers(el).forEach(function (h) { h.setAttribute("aria-expanded", "true"); });
    // the popup is removed on close, its listeners too
    p.addEventListener("click", function (e) {
      const item = e.target.closest(".ah-navbar-item");
      if (!item || !p.contains(item)) { return; }
      const orig = byKey(el, ".ah-navbar-item", "data-key", item.getAttribute("data-key"))[0];
      if (orig) { navbarSelect(el, orig, e); }
      self.popupClose();
    });
    p.addEventListener("keydown", function (e) {
      const item = e.target.closest(".ah-navbar-item");
      if (!item || !p.contains(item)) { return; }
      const items = Array.from(p.querySelectorAll(":scope > .ah-navbar-item"));
      const i = items.indexOf(item);
      const head = el.querySelector(".ah-navbar-header");
      if (e.key === "ArrowDown" || e.key === "ArrowUp") {
        e.preventDefault();
        items[(i + (e.key === "ArrowDown" ? 1 : -1) + items.length) % items.length].focus();
      } else if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        item.click();
        if (head) { head.focus(); }
      } else if (e.key === "Escape") {
        self.popupClose();
        if (head) { head.focus(); }
      }
    });
    return p.querySelector(":scope > .ah-navbar-item-selected") || p.querySelector(":scope > .ah-navbar-item");
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(key) { navbarMark(this.element, key); setValue(this.element, key); }
  select(key) {
    const i = byKey(this.element, ".ah-navbar-item", "data-key", key)[0];
    if (i) { navbarSelect(this.element, i, null); }
  }
  minimize() { this.element.classList.add("ah-navbar-minimized"); }
  restore() { this.element.classList.remove("ah-navbar-minimized"); this.popupClose(); }
});
