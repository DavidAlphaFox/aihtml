/* chip behaviour (designs/04-components.md): remove button, keyboard. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";

var NS = AH.NS;
var L = AH.lib.display;

function removeChip(el, $el) {
  var ev = $.Event("ah:remove");
  $el.trigger(ev, [{ value: $el.attr("data-ah-value") }]);
  if (ev.isDefaultPrevented()) { return false; }
  // value contract: the root's change carries data-ah-value
  $el.trigger("change");
  L.drop(el);
  return true;
}

AH.define("chip", {
  init: function (el, $el) {
    $el.on("click" + NS, ".ah-chip__delete", function (e) {
      e.stopPropagation();
      if ($el.attr("data-disabled") !== "true") { removeChip(el, $el); }
    });
    $el.on("keydown" + NS, function (e) {
      if (e.target !== el || $el.attr("data-disabled") === "true") { return; }
      if ((e.key === "Backspace" || e.key === "Delete") && $el.find(".ah-chip__delete").length) {
        e.preventDefault();
        var $next = $el.next("[tabindex]").length ? $el.next("[tabindex]") : $el.prev("[tabindex]");
        if (removeChip(el, $el)) { $next.trigger("focus"); }
      } else if ($el.attr("data-clickable") === "true") {
        L.keyClick(e);
      }
    });
  },
  methods: {
    remove: function (el, $el) { removeChip(el, $el); }
  }
});
