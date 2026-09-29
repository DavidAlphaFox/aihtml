/* Behaviour of the contribution heatmap (designs/04-components.md).
 *
 * Ported from sigil (data/heatmap_calendar). The grid is server-rendered;
 * this adds the hover tooltip (AH.float) and "ah:select" on click
 * (detail HeatmapSelect: {date, value}).
 */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";

/** Detail of ah:select: the clicked cell's date and value. */
export interface HeatmapSelect { date: string | null; value: number; }

const CELL = ".ah-heatmap-calendar__cell";

class HeatmapCalendarController extends AH.Controller {
  #tip: HTMLElement | null = null;
  #fmt = "{value} · {date}";
  #float: FloatHandle | null = null;

  override setup(): void {
    const el = this.element;
    this.#tip = el.querySelector<HTMLElement>(":scope > .ah-heatmap-calendar__tooltip");
    this.#fmt = el.getAttribute("data-tip") || "{value} · {date}";
    this.#float = null;
    // mouseenter / mouseleave of a cell, from the delegated over / out
    this.delegate("mouseover", CELL, (e, cell) => {
      if (e.relatedTarget instanceof Node && cell.contains(e.relatedTarget)) { return; }
      this.show(cell);
    });
    this.delegate("mouseout", CELL, (e, cell) => {
      if (e.relatedTarget instanceof Node && cell.contains(e.relatedTarget)) { return; }
      this.hide();
    });
    this.delegate("click", CELL, (_e, cell) => {
      const date = cell.getAttribute("data-date");
      el.setAttribute("data-ah-value", date ?? "null");
      this.fire<HeatmapSelect>("ah:select", { date: date, value: parseFloat(cell.getAttribute("data-value") ?? "") });
    });
  }

  override teardown(): void { this.hide(); }

  private show(cell: HTMLElement): void {
    const tip = this.#tip;
    if (!tip) { return; }
    this.hide();
    tip.textContent = this.#fmt.split("{date}").join(cell.getAttribute("data-date") ?? "null")
                               .split("{value}").join(cell.getAttribute("data-value") ?? "null");
    tip.setAttribute("data-visible", "true");
    this.#float = AH.float(tip, cell, { placement: "top", align: "center", offset: 6 });
  }

  private hide(): void {
    if (this.#float) { this.#float.stop(); this.#float = null; }
    if (this.#tip) { this.#tip.setAttribute("data-visible", "false"); }
  }
}

AH.register("heatmap-calendar", HeatmapCalendarController);
