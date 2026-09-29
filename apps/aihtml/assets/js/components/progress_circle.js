/* progress-circle behaviour (designs/04-components.md): setValue /
   getValue. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";
import "./_lib_progress.js";

var num = AH.lib.display.num, clamp = AH.lib.display.clamp;
var CIRC = 2 * Math.PI * 45;

AH.define("progress-circle", {
  methods: {
    setValue: function (el, $el, value) {
      var old = num($el.attr("data-ah-value"), 0);
      var v = Math.trunc(clamp(num(value, 0), 0, 100));
      $el.removeClass("ah-progress-circle--indeterminate").removeAttr("aria-busy");
      $el.find(".ah-progress-circle-fill").attr("stroke-dashoffset", CIRC * (1 - v / 100));
      $el.find(".ah-progress-circle-value").text(v + "%");
      $el.attr({ "data-ah-value": v, "aria-valuenow": v, "aria-valuetext": v + "%" });
      AH.lib.progress.fire($el, old, v, 100);
    },
    getValue: function (el, $el) { return num($el.attr("data-ah-value"), 0); }
  }
});
