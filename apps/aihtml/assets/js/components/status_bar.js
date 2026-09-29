/* Behaviour of the status_bar component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js. */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

// ------------------------------------------------------------------
// status_bar: the count details float above their segment
// ------------------------------------------------------------------

AH.define("status-bar", {
  init: function (el, $el) {
    var show = function () {
      var pop = $(this).children(".ah-status-bar__popover")[0];
      if (!pop) { return; }
      clearTimeout($.data(pop, "ah-hide"));
      if (!$.data(pop, "ah-float")) {
        $.data(pop, "ah-float", AH.float(pop, this, { placement: "top", offset: 8 }));
      }
    };
    var hide = function () {
      var pop = $(this).children(".ah-status-bar__popover")[0];
      if (!pop || this.matches(":hover") || this.contains(document.activeElement)) { return; }
      // keep it in place while the CSS fade-out runs
      $.data(pop, "ah-hide", setTimeout(function () {
        var h = $.data(pop, "ah-float");
        if (h) { h.stop(); $.removeData(pop, "ah-float"); }
      }, 200));
    };
    $el.on("mouseenter" + NS + " focusin" + NS, ".ah-status-bar__count", show);
    $el.on("mouseleave" + NS + " focusout" + NS, ".ah-status-bar__count", hide);
  },
  destroy: function (el, $el) {
    $el.find(".ah-status-bar__popover").each(function () {
      clearTimeout($.data(this, "ah-hide"));
      var h = $.data(this, "ah-float");
      if (h) { h.stop(); }
    });
  }
});
