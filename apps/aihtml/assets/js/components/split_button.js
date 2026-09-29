/* split-button: main action + menu; arrow and menu clicks do not reach
 * the root, so on(click) there is the main action. Choosing an item sets
 * data-ah-value and fires change (designs/04-components.md). The menu
 * code is in _lib_button.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_button.js";

var NS = AH.NS;
var L = AH.lib.button;

var SPLIT = {
  trigger: ".ah-split-button__arrow",
  menu: ".ah-split-button__menu",
  item: ".ah-split-button__item",
  disabled: function ($el) { return $el.attr("data-disabled") === "true"; },
  isOpen: function ($el) { return $el.attr("data-open") === "true"; },
  show: function ($el, on) {
    $el.attr("data-open", on ? "true" : "false");
    if (on) {
      L.floatMenu($el[0], $el.children(".ah-split-button__menu")[0],
                  { placement: "bottom",
                    align: $el.attr("data-menu-align") === "start" ? "start" : "end" });
    } else {
      L.unfloat($el[0], 150);
    }
  },
  select: function () {}
};

AH.define("split-button", {
  init: function (el, $el) {
    L.menuInit(el, $el, SPLIT);
    // Only the main half's clicks reach the root (and its on(click)).
    $el.on("click" + NS, ".ah-split-button__arrow, .ah-split-button__menu", function (e) {
      e.stopPropagation();
    });
  },
  destroy: function (el) { L.menuDestroy(el); },
  methods: {
    open: function (el, $el) { L.menuOpen($el, SPLIT, false); },
    close: function (el, $el) { L.menuClose($el, SPLIT, false); },
    setValue: function (el, $el, v) { L.setValue($el, v, false); },
    getValue: function (el, $el) { return $el.attr("data-ah-value"); }
  }
});
