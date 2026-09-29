/* Behaviour of the input component, also used by textarea
 * (designs/04-components.md). Ported from sigil: form/input. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_input.js";

var NS = AH.NS;
var L = AH.lib.input;

function inputOf($el) {
  return $el.find("input.ah-input, textarea.ah-input").first();
}

function syncInput($el, $input) {
  var filled = $input.val() !== "";
  $el.toggleClass("ah-input-has-value", filled);
  L.syncLabel($el, "ah-input", filled, $input.is(":focus"));
}

function clearInput($el, $input) {
  if ($input.val() === "") { return; }
  $input.val("");
  syncInput($el, $input);
  // Native events, so on(input|change, ...) on the <input> hears them.
  $input.trigger("input").trigger("change");
}

AH.define("input", {
  init: function (el, $el) {
    var $input = inputOf($el);
    L.focusShell($el, $input, "ah-input");
    $input.on("input" + NS, function () { syncInput($el, $input); });
    $el.on("click" + NS, ".ah-input-clear", function (e) {
      e.preventDefault();
      clearInput($el, $input);
      $input.trigger("focus");
    });
    if ($el.hasClass("ah-input-clearable")) {
      $input.on("keydown" + NS, function (e) {
        if (e.key === "Escape" && $input.val() !== "") {
          e.preventDefault();
          clearInput($el, $input);
        }
      });
    }
    syncInput($el, $input);
  },
  methods: {
    getValue: function (el, $el) { return inputOf($el).val(); },
    setValue: function (el, $el, v) {
      var $input = inputOf($el);
      $input.val(v == null ? "" : String(v));
      syncInput($el, $input);
    },
    clear: function (el, $el) { clearInput($el, inputOf($el)); },
    focus: function (el, $el) { inputOf($el).trigger("focus"); },
    selectAll: function (el, $el) {
      var $input = inputOf($el);
      $input.trigger("focus");
      if ($input[0]) { $input[0].select(); }
    }
  }
});
