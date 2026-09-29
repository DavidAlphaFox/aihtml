/* timeline behaviour (designs/04-components.md): cards with a
   description expand on click / Enter. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";

var NS = AH.NS;

AH.define("timeline", {
  init: function (el, $el) {
    $el.on("click" + NS, ".ah-timeline-item[ah-collapsible]", function () {
      var $item = $(this).toggleClass("ah-timeline-item-expanded");
      var on = $item.hasClass("ah-timeline-item-expanded");
      $item.attr("aria-expanded", on ? "true" : "false");
      // three grid cells per item
      var cell = $item.closest(".ah-timeline-near-cell, .ah-timeline-far-cell").index();
      $el.trigger("ah:toggle", [on, Math.floor(cell / 3)]);
    });
    $el.on("keydown" + NS, ".ah-timeline-item[ah-collapsible]", AH.lib.display.keyClick);
  }
});
