/* Behaviour of tabs (value: the active key; fires change). */
import AH from "../core.js";
import "./_lib_layout.js";
import "./_lib_nav.js";

const L = AH.lib.layout;

function tabItems(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-tabs-header > .ah-tabs-item"));
}

function tabPanels(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-tabs-content > .ah-tabs-panel"));
}

function tabIndexOf(el, k) {
  return tabItems(el).findIndex(function (it) { return it.getAttribute("data-key") === String(k); });
}

function tabSelect(el, idx, user) {
  const items = tabItems(el);
  const panels = tabPanels(el);
  const cur = items.findIndex(function (it) { return it.classList.contains("ah-tabs-item-selected"); });
  if (idx < 0 || idx >= items.length || idx === cur ||
      items[idx].classList.contains("ah-tabs-item-disabled")) {
    return false;
  }
  items.forEach(function (it) {
    it.classList.remove("ah-tabs-item-selected");
    it.setAttribute("aria-selected", "false");
    it.setAttribute("tabindex", "-1");
  });
  items[idx].classList.add("ah-tabs-item-selected");
  items[idx].setAttribute("aria-selected", "true");
  items[idx].setAttribute("tabindex", "0");
  panels.forEach(function (p) { p.setAttribute("aria-hidden", "true"); });
  const next = panels[idx];
  if (next) { next.setAttribute("aria-hidden", "false"); }
  const old = cur >= 0 ? (panels[cur] ? [panels[cur]] : [])
    : panels.filter(function (p) { return p !== next; });
  panels.forEach(L.stop);
  if ((el.getAttribute("data-animation") || "fade") === "fade" && old.length && next) {
    let left = old.length;
    old.forEach(function (o) {
      L.fade(o, false, 100, function () {
        o.classList.remove("ah-tabs-panel-active");
        if (--left > 0) { return; }
        L.hide(next);
        L.fade(next, true, 100, function () { next.classList.add("ah-tabs-panel-active"); });
      });
    });
  } else {
    old.forEach(function (o) {
      L.hide(o);
      o.classList.remove("ah-tabs-panel-active");
    });
    if (next) {
      next.style.display = "";
      next.classList.add("ah-tabs-panel-active");
    }
  }
  L.setValue(el, items[idx].getAttribute("data-key"));
  if (user) {
    el.dispatchEvent(new CustomEvent("change", { bubbles: true, cancelable: true }));
  }
  return true;
}

AH.register("tabs", class extends AH.Controller {
  setup() {
    const el = this.element;
    const header = el.querySelector(":scope > .ah-tabs-header");
    if (!header) { return; }
    const choose = function (item) {
      if (!el.classList.contains("ah-tabs-disabled")) {
        tabSelect(el, tabItems(el).indexOf(item), true);
      }
    };
    if (el.getAttribute("data-selection-mode") === "hover") {
      AH.lib.nav.hover(this, ".ah-tabs-item", choose, null, header);
    } else {
      this.delegate("click", ".ah-tabs-item", function (e, item) { choose(item); }, header);
    }
    this.delegate("keydown", ".ah-tabs-item", function (e, item) {
      const items = tabItems(el);
      const vertical = el.classList.contains("ah-tabs-left") || el.classList.contains("ah-tabs-right");
      const t = L.listKeys(e, items, items.indexOf(item), vertical, "ah-tabs-item-disabled");
      if (t === null) { return; }
      e.preventDefault();
      tabSelect(el, t, true);
      items[t].focus();
    }, header);
    this.delegate("click", ".ah-tabs-scroll-btn", function (e, btn) {
      const step = btn.classList.contains("ah-tabs-scroll-left") ? -80 : 80;
      header.scrollLeft = header.scrollLeft + step;
    }, header);
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  select(k) { tabSelect(this.element, tabIndexOf(this.element, k), false); }
  disable(k) {
    const it = tabItems(this.element)[tabIndexOf(this.element, k)];
    if (it) {
      it.classList.add("ah-tabs-item-disabled");
      it.setAttribute("aria-disabled", "true");
    }
  }
  enable(k) {
    const it = tabItems(this.element)[tabIndexOf(this.element, k)];
    if (it) {
      it.classList.remove("ah-tabs-item-disabled");
      it.removeAttribute("aria-disabled");
    }
  }
  value() { return this.element.getAttribute("data-ah-value"); }
});
