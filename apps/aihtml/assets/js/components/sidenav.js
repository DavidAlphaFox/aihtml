/* Behaviour of the sidenav component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js.
   Value contract: data-ah-value (the active link's data-route), the
   hidden input and "change" on the root. ah:collapse fires on the root
   when it collapses or expands, detail {collapsed: boolean}. */
import AH from "../core.js";
import "./_lib_nav.js";

const N = AH.lib.nav;
const visible = N.visible;
const setValue = N.setValue;
const byKey = N.byKey;

// ------------------------------------------------------------------
// sidenav (sigil sidenav.cljs + nav-tree)
// ------------------------------------------------------------------

function summaries(details) {
  return details.querySelectorAll(":scope > summary");
}

function sidenavMark(el, key) {
  el.querySelectorAll(".ah-nav-tree__item.ah-is-active").forEach(function (n) {
    n.classList.remove("ah-is-active");
    n.removeAttribute("aria-current");
  });
  byKey(el, "a.ah-nav-tree__item", "data-route", key).forEach(function (a) {
    a.classList.add("ah-is-active");
    a.setAttribute("aria-current", "page");
    for (let d = a.parentNode; d && d !== el; d = d.parentNode) {
      if (d.matches("details.ah-nav-tree__node")) {
        d.open = true;
        summaries(d).forEach(function (s) { s.classList.add("ah-is-open"); });
      }
    }
  });
}

function sidenavCollapse(el, collapsed) {
  el.classList.toggle("ah-sidenav-collapsed", collapsed);
  el.querySelectorAll(".ah-sidenav__toggle").forEach(function (t) {
    t.setAttribute("aria-expanded", String(!collapsed));
  });
  el.dispatchEvent(new CustomEvent("ah:collapse",
                                   { bubbles: true, cancelable: true, detail: { collapsed: collapsed } }));
}

AH.register("sidenav", class extends AH.Controller {
  setup() {
    const el = this.element;
    this.delegate("click", "a.ah-nav-tree__item", function (e, a) {
      if (a.getAttribute("aria-disabled") === "true") { e.preventDefault(); return; }
      const key = a.getAttribute("data-route");
      sidenavMark(el, key);
      if (a.getAttribute("href") === "#") {
        e.preventDefault();
        if (el.getAttribute("data-ah-value") !== key) { setValue(el, key, "change"); }
      }
    });
    // a collapsed sidebar expands when a group is opened
    this.delegate("click", "summary.ah-nav-tree__item", function (e, s) {
      if (el.classList.contains("ah-sidenav-collapsed")) {
        e.preventDefault();
        sidenavCollapse(el, false);
        s.parentNode.open = true;
        s.classList.add("ah-is-open");
      }
    });
    // toggle does not bubble: listen in the capture phase
    this.listen(el, "toggle", function (e) {
      if (e.target.tagName === "DETAILS") {
        summaries(e.target).forEach(function (s) { s.classList.toggle("ah-is-open", e.target.open); });
      }
    }, { capture: true });
    this.delegate("click", ".ah-sidenav__toggle", function () {
      sidenavCollapse(el, !el.classList.contains("ah-sidenav-collapsed"));
    });
    // arrow keys move between the visible entries
    this.delegate("keydown", ".ah-nav-tree__item", function (e, cur) {
      if (!/^(ArrowDown|ArrowUp|Home|End)$/.test(e.key)) { return; }
      const items = Array.from(el.querySelectorAll(".ah-nav-tree__item")).filter(visible);
      const i = items.indexOf(cur);
      const to = e.key === "Home" ? 0 : e.key === "End" ? items.length - 1
        : Math.max(0, Math.min(items.length - 1, i + (e.key === "ArrowDown" ? 1 : -1)));
      e.preventDefault();
      if (items[to]) { items[to].focus(); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(key) { sidenavMark(this.element, key); setValue(this.element, key); }
  collapse() { sidenavCollapse(this.element, true); }
  expand() { sidenavCollapse(this.element, false); }
  toggle() { sidenavCollapse(this.element, !this.element.classList.contains("ah-sidenav-collapsed")); }
});
