/* kpi-card behaviour (designs/04-components.md): setValue / setTrend. */
import AH from "../core.js";
import "./_lib_display.js";

const num = AH.lib.display.num;
const UP = "ah-kpi-card-trend-up", DOWN = "ah-kpi-card-trend-down";

AH.register("kpi-card", class extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setValue(value) {
    this.element.querySelectorAll(".ah-kpi-card-value").forEach((n) => { n.textContent = String(value); });
  }

  setTrend(trend) {
    const el = this.element;
    const t = num(trend, NaN);
    const ts = el.querySelectorAll(".ah-kpi-card-trend");
    if (isNaN(t) || !ts.length) { return; }
    const up = t > 0, cls = up ? UP : DOWN;
    el.classList.remove(UP, DOWN);
    el.classList.add(cls);
    const pts = [["23 6 13.5 15.5 8.5 10.5 1 18", "17 6 23 6 23 12"],
                 ["23 18 13.5 8.5 8.5 13.5 1 6", "17 18 23 18 23 12"]][up ? 0 : 1];
    ts.forEach((tr) => {
      const first = tr.firstElementChild;
      if (first) { first.classList.remove(UP, DOWN); first.classList.add(cls); }
      tr.querySelectorAll(".ah-kpi-card-trend-value").forEach((n) => {
        n.textContent = (up ? "+" : "") + t.toFixed(1) + "%";
      });
      // swap the arrow: mirror the polylines vertically
      tr.querySelectorAll(".ah-kpi-card-trend-icon polyline").forEach((p, i) => {
        p.setAttribute("points", pts[i] || pts[0]);
      });
    });
  }
});
