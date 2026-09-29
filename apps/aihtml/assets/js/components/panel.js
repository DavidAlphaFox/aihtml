/* Behaviour of panel: a scroll area with an optional collapsible header. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;


  function panelSet(el, $el, open, user) {
    var $w = $el.children(".ah-panel-wrapper");
    var $t = $el.children(".ah-panel-header").children(".ah-panel-toggle");
    if (!$t.length || ($t.attr("aria-expanded") === "true") === open) {
      return;
    }
    $t.attr("aria-expanded", String(open));
    $el.toggleClass("ah-panel-collapsed", !open);
    $w.stop(true, true)[open ? "slideDown" : "slideUp"](200, function () {
      if (user) {
        $el.trigger(open ? "ah:expand" : "ah:collapse");
      }
    });
  }

  AH.define("panel", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-panel-toggle", function (e) {
        if ($(this).closest(".ah-panel")[0] !== el) {
          return;
        }
        e.preventDefault();
        panelSet(el, $el, this.getAttribute("aria-expanded") !== "true", true);
      });
    },
    methods: {
      scrollTo: function (el, $el, x, y) {
        var w = $el.children(".ah-panel-wrapper")[0];
        if (w) {
          w.scrollLeft = x || 0;
          w.scrollTop = y || 0;
        }
      },
      refresh: function () { /* native scrolling: nothing to measure */ },
      collapse: function (el, $el) { panelSet(el, $el, false, false); },
      expand: function (el, $el) { panelSet(el, $el, true, false); },
      toggle: function (el, $el) {
        panelSet(el, $el, $el.hasClass("ah-panel-collapsed"), false);
      }
    }
  });
})(window.jQuery, window.AH);
