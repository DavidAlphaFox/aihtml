/* Helpers shared by the progressbar and progress-circle behaviours.
   Internal: AH.lib.progress. */
import $ from "jquery";
import AH from "../core.js";

AH.lib = AH.lib || {};
AH.lib.progress = {
  pct: function (v, lo, hi) {
    return hi > lo ? 100 * (v - lo) / (hi - lo) : 0;
  },

  // change on every new value, ah:complete when it reaches max
  fire: function ($el, old, v, max) {
    if (old === v) { return; }
    $el.trigger("change", [{ previous: old, value: v }]);
    if (v === max) { $el.trigger("ah:complete", [{ previous: old, value: v }]); }
  }
};
