/* Behaviour of the contribution heatmap (designs/04-components.md).
 *
 * Ported from sigil (data/heatmap_calendar). The grid is server-rendered;
 * this adds the hover tooltip (AH.float) and ah:select on click.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  AH.define("heatmap-calendar", {
    init: function (el, $el) {
      var tip = $el.children(".ah-heatmap-calendar__tooltip")[0];
      var fmt = el.getAttribute("data-tip") || "{value} · {date}";
      var st = { float: null };
      $.data(el, "ah-heatmap", st);
      function hide() {
        if (st.float) { st.float.stop(); st.float = null; }
        if (tip) { tip.setAttribute("data-visible", "false"); }
      }
      $el.on("mouseenter" + NS, ".ah-heatmap-calendar__cell", function () {
        if (!tip) { return; }
        hide();
        tip.textContent = fmt.split("{date}").join(this.getAttribute("data-date"))
                             .split("{value}").join(this.getAttribute("data-value"));
        tip.setAttribute("data-visible", "true");
        st.float = AH.float(tip, this, { placement: "top", align: "center", offset: 6 });
      });
      $el.on("mouseleave" + NS, ".ah-heatmap-calendar__cell", hide);
      $el.on("click" + NS, ".ah-heatmap-calendar__cell", function () {
        var date = this.getAttribute("data-date");
        el.setAttribute("data-ah-value", date);
        $el.trigger("ah:select", [{ date: date, value: parseFloat(this.getAttribute("data-value")) }]);
      });
    },
    destroy: function (el) {
      var st = $.data(el, "ah-heatmap");
      if (st && st.float) { st.float.stop(); }
    }
  });
})(window.jQuery, window.AH);
