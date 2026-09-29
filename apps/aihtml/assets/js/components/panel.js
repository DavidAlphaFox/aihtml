/* Behaviour of panel: a scroll area with an optional collapsible header.
 * A user toggle fires ah:expand / ah:collapse (no detail) on the root
 * when the slide ends. */
import AH from "../core.js";
import "./_lib_layout.js";

const L = AH.lib.layout;

function panelSet(el, open, user) {
  const w = el.querySelector(":scope > .ah-panel-wrapper");
  const t = el.querySelector(":scope > .ah-panel-header > .ah-panel-toggle");
  if (!t || (t.getAttribute("aria-expanded") === "true") === open) {
    return;
  }
  t.setAttribute("aria-expanded", String(open));
  el.classList.toggle("ah-panel-collapsed", !open);
  const done = function () {
    if (user) {
      el.dispatchEvent(new CustomEvent(open ? "ah:expand" : "ah:collapse",
                                       { bubbles: true, cancelable: true }));
    }
  };
  if (w) { L.slide(w, open, 200, done); } else { done(); }
}

AH.register("panel", class extends AH.Controller {
  setup() {
    const el = this.element;
    this.delegate("click", ".ah-panel-toggle", function (e, t) {
      if (t.closest(".ah-panel") !== el) {
        return;
      }
      e.preventDefault();
      panelSet(el, t.getAttribute("aria-expanded") !== "true", true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  scrollTo(x, y) {
    const w = this.element.querySelector(":scope > .ah-panel-wrapper");
    if (w) {
      w.scrollLeft = x || 0;
      w.scrollTop = y || 0;
    }
  }
  refresh() { /* native scrolling: nothing to measure */ }
  collapse() { panelSet(this.element, false, false); }
  expand() { panelSet(this.element, true, false); }
  toggle() { panelSet(this.element, this.element.classList.contains("ah-panel-collapsed"), false); }
});
