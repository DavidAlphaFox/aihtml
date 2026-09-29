/* ranking-list behaviour (designs/04-components.md): clickable rows. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";

var NS = AH.NS;
var L = AH.lib.display;

AH.define("ranking-list", {
  init: function (el, $el) {
    $el.on("click" + NS, ".ah-ranking-list__item--clickable", function () {
      $el.trigger("ah:item-click", [{ index: L.num(this.getAttribute("data-idx"), 0) }]);
    });
    $el.on("keydown" + NS, ".ah-ranking-list__item--clickable", L.keyClick);
  }
});
