/* dropdown-button: a menu popup (outside click / Escape close, arrow
 * keys); choosing an item sets data-ah-value and fires change (detail:
 * the item's value); "ah:open" / "ah:close" (no detail)
 * (designs/04-components.md). The menu code is in _lib_button.js. */
import AH from "../core.js";
import "./_lib_button.js";

var L = AH.lib.button;

function popupOf(el) { return el.querySelector(":scope > .ah-dropdown-btn-popup"); }

var DROPDOWN = {
  trigger: ".ah-dropdown-btn-wrapper",
  menu: ".ah-dropdown-btn-popup",
  item: ".ah-dropdown-btn-item",
  disabled: function (el) { return el.classList.contains("ah-dropdown-btn-disabled"); },
  isOpen: function (el) { return el.classList.contains("ah-dropdown-btn-opened"); },
  show: function (el, on) {
    var popup = popupOf(el);
    el.classList.toggle("ah-dropdown-btn-opened", on);
    if (!popup) { return; }
    if (on) {
      popup.removeAttribute("hidden");
      L.floatMenu(el, popup, { placement: "bottom", align: "start", matchWidth: true });
    } else {
      popup.setAttribute("hidden", "");
      L.unfloat(el, 0);
    }
  },
  select: function (el, item) {
    el.querySelectorAll(".ah-dropdown-btn-item").forEach(function (i) { i.classList.remove("selected"); });
    item.classList.add("selected");
  }
};

AH.register("dropdown-button", class extends AH.Controller {
  setup() {
    var el = this.element;
    L.menuInit(this, DROPDOWN);
    this.listen(el, "mouseenter", () => {
      if (DROPDOWN.disabled(el)) { return; }
      el.classList.add("ah-dropdown-btn-hover");
      if (el.classList.contains("ah-dropdown-btn-auto-open")) { L.menuOpen(el, DROPDOWN, false); }
    });
    this.listen(el, "mouseleave", () => {
      el.classList.remove("ah-dropdown-btn-hover");
      if (el.classList.contains("ah-dropdown-btn-auto-open")) { L.menuClose(el, DROPDOWN, false); }
    });
    var trigger = el.querySelector(":scope > .ah-dropdown-btn-wrapper");
    if (trigger) {
      this.listen(trigger, "focus", () => {
        // sigil shows the focus ring on focus; keep it to keyboard focus
        var visible = true;
        try { visible = trigger.matches(":focus-visible"); } catch (err) { /* old browser */ }
        if (visible) { el.classList.add("ah-dropdown-btn-focused"); }
      });
      this.listen(trigger, "blur", () => { el.classList.remove("ah-dropdown-btn-focused"); });
    }
  }

  teardown() { L.menuDestroy(this.element); }

  // methods (aihtml_action:call/4, AH.invoke)
  open() { L.menuOpen(this.element, DROPDOWN, false); }
  close() { L.menuClose(this.element, DROPDOWN, false); }
  toggle() {
    if (DROPDOWN.isOpen(this.element)) { this.close(); } else { this.open(); }
  }
  setValue(v) {
    var el = this.element;
    el.querySelectorAll(".ah-dropdown-btn-item").forEach(function (i) {
      i.classList.toggle("selected", i.getAttribute("data-value") === String(v));
    });
    L.setValue(el, v, false);
  }
  getValue() { return this.element.getAttribute("data-ah-value"); }
});
