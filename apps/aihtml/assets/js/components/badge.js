/* badge behaviour (designs/04-components.md): setCount(n) with the same
   max / zero rules as the server. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";

var num = AH.lib.display.num;

AH.define("badge", {
  methods: {
    setCount: function (el, $el, n) {
      var $ind = $el.children(".ah-badge-indicator");
      if ($ind.attr("data-dot") === "true") { return; }
      var max = num($el.attr("data-ah-max"), 99);
      var isNum = typeof n === "number" || (n !== "" && n !== null && !isNaN(n));
      var v = isNum ? Number(n) : n;
      $ind.text(v === null || v === undefined ? "" : (isNum && v > max ? max + "+" : String(v)));
      var hide = isNum && v === 0 && $el.attr("data-ah-show-zero") !== "true";
      $ind.attr("data-invisible", hide ? "true" : "false");
    }
  }
});
