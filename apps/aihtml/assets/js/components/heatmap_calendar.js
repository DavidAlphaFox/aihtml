/* Behaviour of the contribution heatmap (designs/04-components.md).
 *
 * Ported from sigil (data/heatmap_calendar). The grid is server-rendered;
 * this adds the hover tooltip (AH.float) and "ah:select" on click
 * (detail {date, value}).
 */
import AH from "../core.js";

const CELL = ".ah-heatmap-calendar__cell";

AH.register("heatmap-calendar", class extends AH.Controller {
  setup() {
    const el = this.element;
    this.tip = el.querySelector(":scope > .ah-heatmap-calendar__tooltip");
    this.fmt = el.getAttribute("data-tip") || "{value} · {date}";
    this.float = null;
    // mouseenter / mouseleave of a cell, from the delegated over / out
    this.delegate("mouseover", CELL, (e, cell) => {
      if (e.relatedTarget && cell.contains(e.relatedTarget)) { return; }
      this.show(cell);
    });
    this.delegate("mouseout", CELL, (e, cell) => {
      if (e.relatedTarget && cell.contains(e.relatedTarget)) { return; }
      this.hide();
    });
    this.delegate("click", CELL, (e, cell) => {
      const date = cell.getAttribute("data-date");
      el.setAttribute("data-ah-value", date);
      this.fire("ah:select", { date: date, value: parseFloat(cell.getAttribute("data-value")) });
    });
  }

  teardown() { this.hide(); }

  show(cell) {
    const tip = this.tip;
    if (!tip) { return; }
    this.hide();
    tip.textContent = this.fmt.split("{date}").join(cell.getAttribute("data-date"))
                              .split("{value}").join(cell.getAttribute("data-value"));
    tip.setAttribute("data-visible", "true");
    this.float = AH.float(tip, cell, { placement: "top", align: "center", offset: 6 });
  }

  hide() {
    if (this.float) { this.float.stop(); this.float = null; }
    if (this.tip) { this.tip.setAttribute("data-visible", "false"); }
  }
});
