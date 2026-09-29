/* split-button: main action + menu; arrow and menu clicks do not reach
 * the root, so on(click) there is the main action. Choosing an item sets
 * data-ah-value and fires change (detail: the item's value); "ah:open" /
 * "ah:close" (no detail) (designs/04-components.md). The menu code is in
 * _lib_button.js. */
import AH from "../core.js";
import "./_lib_button.js";

var L = AH.lib.button;

var SPLIT = {
  trigger: ".ah-split-button__arrow",
  menu: ".ah-split-button__menu",
  item: ".ah-split-button__item",
  disabled: function (el) { return el.getAttribute("data-disabled") === "true"; },
  isOpen: function (el) { return el.getAttribute("data-open") === "true"; },
  show: function (el, on) {
    el.setAttribute("data-open", on ? "true" : "false");
    var menu = el.querySelector(":scope > .ah-split-button__menu");
    if (on) {
      if (menu) {
        L.floatMenu(el, menu, { placement: "bottom",
                                align: el.getAttribute("data-menu-align") === "start" ? "start" : "end" });
      }
    } else {
      L.unfloat(el, 150);
    }
  },
  select: function () {}
};

AH.register("split-button", class extends AH.Controller {
  setup() {
    L.menuInit(this, SPLIT);
    // Only the main half's clicks reach the root (and its on(click)).
    this.element.querySelectorAll(".ah-split-button__arrow, .ah-split-button__menu").forEach((n) => {
      this.listen(n, "click", (e) => { e.stopPropagation(); });
    });
  }

  teardown() { L.menuDestroy(this.element); }

  // methods (aihtml_action:call/4, AH.invoke)
  open() { L.menuOpen(this.element, SPLIT, false); }
  close() { L.menuClose(this.element, SPLIT, false); }
  setValue(v) { L.setValue(this.element, v, false); }
  getValue() { return this.element.getAttribute("data-ah-value"); }
});
