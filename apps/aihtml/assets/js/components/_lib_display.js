/* Helpers shared by the display component behaviours (chip, badge,
   progressbar, progress-circle, kpi-card, timeline, ranking-list,
   tag-cloud, alert). Internal: AH.lib.display. */
(function ($, AH) {
  "use strict";

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
      $(el).remove();
    },

    // Enter / Space act as a click on focusable non-button elements.
    keyClick: function (e) {
      if (e.key === "Enter" || e.key === " ") {
        e.preventDefault();
        $(e.currentTarget).trigger("click");
      }
    }
  };
})(window.jQuery, window.AH);
