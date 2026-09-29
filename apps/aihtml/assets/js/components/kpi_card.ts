/* kpi-card behaviour (designs/04-components.md): setValue / setTrend. */
import AH from "../core.ts";
import { num } from "./_lib_display.ts";

const UP = "ah-kpi-card-trend-up", DOWN = "ah-kpi-card-trend-down";
// the arrow's polylines, pointing up and down
const POINTS = [["23 6 13.5 15.5 8.5 10.5 1 18", "17 6 23 6 23 12"],
                ["23 18 13.5 8.5 8.5 13.5 1 6", "17 18 23 18 23 12"]];

class KpiCardController extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setValue(value: unknown): void {
    this.element.querySelectorAll(".ah-kpi-card-value").forEach((n) => { n.textContent = String(value); });
  }

  /** the change in percent; the sign picks the arrow */
  setTrend(trend: unknown): void {
    const el = this.element;
    const t = num(trend, NaN);
    const ts = el.querySelectorAll(".ah-kpi-card-trend");
    if (isNaN(t) || !ts.length) { return; }
    const up = t > 0, cls = up ? UP : DOWN;
    el.classList.remove(UP, DOWN);
    el.classList.add(cls);
    const pts = POINTS[up ? 0 : 1];
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
}

AH.register("kpi-card", KpiCardController);
