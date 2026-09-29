/* expandable-text behaviour (designs/04-components.md): the toggle swaps
   the cut and the full text. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setExpanded(el, $el, on) {
    var $btn = $el.children(".ah-expandable-text__toggle");
    if (!$btn.length || ($el.attr("data-expanded") === "true") === on) { return; }
    $el.attr("data-expanded", on ? "true" : "false");
    $el.find("[data-ah-part=short]").prop("hidden", on);
    $el.find("[data-ah-part=full]").prop("hidden", !on);
    $btn.attr("aria-expanded", on ? "true" : "false")
      .text($btn.attr(on ? "data-ah-collapse-label" : "data-ah-expand-label"));
    $el.trigger("ah:toggle", [on]);
  }

  AH.define("expandable-text", {
    init: function (el, $el) {
      $el.on("click" + NS, ".ah-expandable-text__toggle", function () {
        setExpanded(el, $el, $el.attr("data-expanded") !== "true");
      });
    },
    methods: {
      toggle: function (el, $el) { setExpanded(el, $el, $el.attr("data-expanded") !== "true"); },
      expand: function (el, $el) { setExpanded(el, $el, true); },
      collapse: function (el, $el) { setExpanded(el, $el, false); }
    }
  });
})(window.jQuery, window.AH);
