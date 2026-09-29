/* toggle-button: a click toggles aria-pressed / data-ah-value and fires
 * change on the root (designs/04-components.md). */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_button.js";

var NS = AH.NS;
var L = AH.lib.button;

function setPressed(el, $el, on, fire) {
  var v = on ? "true" : "false";
  $el.toggleClass("ah-btn-toggled", on).attr("aria-pressed", v).val(v);
  L.setValue($el, v, fire);
}

AH.define("toggle-button", {
  init: function (el, $el) {
    $el.on("click" + NS, function () {
      if (el.disabled) { return; }
      setPressed(el, $el, $el.attr("aria-pressed") !== "true", true);
    });
  },
  methods: {
    toggle: function (el, $el) { setPressed(el, $el, $el.attr("aria-pressed") !== "true", false); },
    setValue: function (el, $el, v) { setPressed(el, $el, v === true || v === "true", false); },
    getValue: function (el, $el) { return $el.attr("aria-pressed") === "true"; }
  }
});
