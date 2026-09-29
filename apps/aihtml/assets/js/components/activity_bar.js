/* Behaviour of the activity bar (designs/04-components.md), ported from
 * sigil's layout/activity_bar. The value (the active item) is kept in
 * data-ah-value on the root, mirrored into a hidden input, and "change"
 * fires when the user changes it; ah:select fires on every user choice,
 * detail = the item's id. Methods called by the server (AH.invoke /
 * aihtml_action:call) do not fire "change". */
import AH from "../core.js";

function setValue(el, v) {
  el.setAttribute("data-ah-value", v);
  const hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
}

// ------------------------------------------------------------------
// ActivityBar: a vertical tablist of icon buttons
// ------------------------------------------------------------------

function barItems(el) {
  return Array.from(el.querySelectorAll(":scope > .ah-activity-bar__item"));
}

function enabled(el) {
  return barItems(el).filter(function (it) { return it.getAttribute("data-disabled") !== "true"; });
}

function barActivate(el, id) {
  const items = barItems(el);
  items.forEach(function (it) {
    const on = it.getAttribute("data-id") === String(id);
    it.setAttribute("data-active", String(on));
    it.setAttribute("aria-selected", String(on));
    it.setAttribute("tabindex", on ? "0" : "-1");
  });
  // keep one item reachable with Tab when nothing is active
  if (!items.some(function (it) { return it.getAttribute("tabindex") === "0"; })) {
    const first = enabled(el)[0];
    if (first) { first.setAttribute("tabindex", "0"); }
  }
  setValue(el, id == null ? "" : String(id));
}

AH.register("activity-bar", class extends AH.Controller {
  setup() {
    const el = this.element;
    const self = this;
    this.delegate("click", ".ah-activity-bar__item", function (e, item) {
      self.choose(item);
    });
    // WAI-ARIA tabs: arrows move and activate, Home / End jump
    this.delegate("keydown", ".ah-activity-bar__item", function (e, item) {
      const en = enabled(el);
      const i = en.indexOf(item);
      let next;
      switch (e.key) {
        case "ArrowDown": case "ArrowRight": next = (i + 1) % en.length; break;
        case "ArrowUp": case "ArrowLeft": next = (i - 1 + en.length) % en.length; break;
        case "Home": next = 0; break;
        case "End": next = en.length - 1; break;
        default: return;
      }
      e.preventDefault();
      const t = en[next];
      if (t) {
        t.focus();
        self.choose(t);
      }
    });
  }

  choose(item) {
    const el = this.element;
    if (item.getAttribute("data-disabled") === "true") { return; }
    const id = item.getAttribute("data-id");
    const changed = el.getAttribute("data-ah-value") !== id;
    barActivate(el, id);
    this.fire("ah:select", id);
    if (changed) { this.fire("change"); }
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(v) { barActivate(this.element, v); }
  getValue() { return this.element.getAttribute("data-ah-value"); }
});
