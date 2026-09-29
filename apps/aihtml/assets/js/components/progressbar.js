/* progressbar behaviour (designs/04-components.md): setValue / getValue. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";
import "./_lib_progress.js";

var num = AH.lib.display.num, clamp = AH.lib.display.clamp;
var P = AH.lib.progress;

AH.define("progressbar", {
  methods: {
    setValue: function (el, $el, value, text) {
      var lo = num($el.attr("data-ah-min"), 0), hi = num($el.attr("data-ah-max"), 100);
      var old = num($el.attr("data-ah-value"), lo);
      var v = clamp(num(value, lo), lo, hi);
      var p = P.pct(v, lo, hi);
      var dim = $el.hasClass("ah-progressbar-vertical") ? "height" : "width";
      $el.removeClass("ah-progressbar-indeterminate").removeAttr("aria-busy");
      $el.children(".ah-progressbar-value, .ah-progressbar-value-vertical").css(dim, p + "%");
      $el.children(".ah-progressbar-range").each(function () {
        var stop = num(this.getAttribute("data-ah-stop"), hi);
        $(this).css(dim, P.pct(Math.min(stop, hi, v), lo, hi) + "%");
      });
      var label = text !== undefined && text !== null ? String(text)
        : ($el.attr("data-ah-text") === "custom" ? null : Math.round(p) + "%");
      if (label !== null) { $el.find(".ah-progressbar-text").text(label); }
      $el.attr({ "data-ah-value": v, "aria-valuenow": v,
                 "aria-valuetext": label !== null ? label : Math.round(p) + "%" });
      P.fire($el, old, v, hi);
    },
    getValue: function (el, $el) { return num($el.attr("data-ah-value"), 0); }
  }
});
