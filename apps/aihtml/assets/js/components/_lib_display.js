/* Helpers shared by the display component behaviours (chip, badge,
   progressbar, progress-circle, kpi-card, timeline, ranking-list,
   tag-cloud, alert). Internal: AH.lib.display. */
import AH from "../core.js";

AH.lib = AH.lib || {};
AH.lib.display = {
  // parseFloat with a default for anything that is not a number
  num: function (v, dflt) {
    var n = parseFloat(v);
    return isNaN(n) ? dflt : n;
  },

  clamp: function (v, lo, hi) {
    return Math.max(lo, Math.min(hi, v));
  },

  // Remove an element the way the runtime does (behaviours destroyed first).
  drop: function (el) {
    AH.destroy(el);
    el.remove();
  },

  // Enter / Space act as a click on focusable non-button elements.
  // target: the element that acts (default e.currentTarget), so it can be
  // used directly as a delegate() handler (handler(e, match)).
  keyClick: function (e, target) {
    if (e.key === "Enter" || e.key === " ") {
      e.preventDefault();
      (target || e.currentTarget).click();
    }
  }
};
