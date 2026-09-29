/* Helpers shared by the progressbar and progress-circle behaviours.
   Internal: AH.lib.progress. */
import AH from "../core.js";

function emit(el, type, detail) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

AH.lib = AH.lib || {};
AH.lib.progress = {
  pct: function (v, lo, hi) {
    return hi > lo ? 100 * (v - lo) / (hi - lo) : 0;
  },

  // change on every new value, ah:complete when it reaches max; both with
  // detail {previous, value}
  fire: function (el, old, v, max) {
    if (old === v) { return; }
    emit(el, "change", { previous: old, value: v });
    if (v === max) { emit(el, "ah:complete", { previous: old, value: v }); }
  }
};
