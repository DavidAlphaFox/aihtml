/* Behaviour of tab-bar (value: the active id; fires change, and ah:close
 * with detail = the closed tab's id). */
import AH from "../core.js";
import "./_lib_layout.js";

const L = AH.lib.layout;

function fire(el, type, detail) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

function barTabs(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-tab-bar__tab"));
}

function barFind(el, id) {
  return barTabs(el).find(function (t) { return t.getAttribute("data-id") === String(id); }) || null;
}

function barSelect(el, tab, user) {
  if (!tab || tab.getAttribute("data-active") === "true") {
    return;
  }
  barTabs(el).forEach(function (t) {
    t.setAttribute("data-active", "false");
    t.setAttribute("aria-selected", "false");
    t.setAttribute("tabindex", "-1");
  });
  tab.setAttribute("data-active", "true");
  tab.setAttribute("aria-selected", "true");
  tab.setAttribute("tabindex", "0");
  L.setValue(el, tab.getAttribute("data-id"));
  if (user) {
    fire(el, "change");
  }
}

function barClose(el, tab, user) {
  if (!tab) {
    return;
  }
  const id = tab.getAttribute("data-id");
  const idx = barTabs(el).indexOf(tab);
  const wasActive = tab.getAttribute("data-active") === "true";
  const hadFocus = tab.contains(document.activeElement);
  tab.remove();
  fire(el, "ah:close", id);
  if (wasActive) {
    const left = barTabs(el);
    if (left.length) {
      const next = left[Math.min(idx, left.length - 1)];
      barSelect(el, next, user);
      if (hadFocus) { next.focus(); }
    } else {
      L.setValue(el, "");
      if (user) { fire(el, "change"); }
    }
  }
}

AH.register("tab-bar", class extends AH.Controller {
  setup() {
    const el = this.element;
    this.delegate("click", ".ah-tab-bar__close", function (e, btn) {
      e.stopPropagation();
      barClose(el, btn.closest(".ah-tab-bar__tab"), true);
    });
    this.delegate("click", ".ah-tab-bar__tab", function (e, tab) {
      // the close button's stopPropagation does not stop this listener
      // (same root), so skip its clicks here
      if (e.target.closest(".ah-tab-bar__close")) { return; }
      barSelect(el, tab, true);
    });
    this.delegate("keydown", ".ah-tab-bar__tab", function (e, tab) {
      const items = barTabs(el);
      const cur = items.indexOf(tab);
      if (L.key(e) === "Delete" && tab.querySelector(":scope > .ah-tab-bar__close")) {
        e.preventDefault();
        barClose(el, tab, true);
        return;
      }
      const t = L.listKeys(e, items, cur, false, "ah-tab-bar__tab--none");
      if (t === null) { return; }
      e.preventDefault();
      barSelect(el, items[t], true);
      items[t].focus();
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  select(id) { barSelect(this.element, barFind(this.element, id), false); }
  close(id) { barClose(this.element, barFind(this.element, id), false); }
  value() { return this.element.getAttribute("data-ah-value"); }
});
