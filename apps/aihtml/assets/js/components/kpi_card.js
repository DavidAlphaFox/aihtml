/* kpi-card behaviour (designs/04-components.md): setValue / setTrend. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_display.js";

var num = AH.lib.display.num;

AH.define("kpi-card", {
  methods: {
    setValue: function (el, $el, value) {
      $el.find(".ah-kpi-card-value").text(String(value));
    },
    setTrend: function (el, $el, trend) {
      var t = num(trend, NaN);
      var $t = $el.find(".ah-kpi-card-trend");
      if (isNaN(t) || !$t.length) { return; }
      var up = t > 0, cls = up ? "ah-kpi-card-trend-up" : "ah-kpi-card-trend-down";
      $el.removeClass("ah-kpi-card-trend-up ah-kpi-card-trend-down").addClass(cls);
      $t.children().first().removeClass("ah-kpi-card-trend-up ah-kpi-card-trend-down")
        .addClass(cls);
      $t.find(".ah-kpi-card-trend-value").text((up ? "+" : "") + t.toFixed(1) + "%");
      // swap the arrow: mirror the polylines vertically
      $t.find(".ah-kpi-card-trend-icon polyline").each(function (i) {
        var pts = [["23 6 13.5 15.5 8.5 10.5 1 18", "17 6 23 6 23 12"],
                   ["23 18 13.5 8.5 8.5 13.5 1 6", "17 18 23 18 23 12"]][up ? 0 : 1];
        this.setAttribute("points", pts[i] || pts[0]);
      });
    }
  }
});
