/* timeline behaviour (designs/04-components.md): cards with a
   description expand on click / Enter; "ah:toggle" with detail
   TimelineToggle {expanded, index} (index: the item's position in the
   timeline). */
import AH from "../core.ts";
import { keyClick } from "./_lib_display.ts";

/** Detail of ah:toggle. */
export interface TimelineToggle { expanded: boolean; index: number; }

const ITEM = ".ah-timeline-item[ah-collapsible]";

class TimelineController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ITEM, (_e, item) => {
      const on = item.classList.toggle("ah-timeline-item-expanded");
      item.setAttribute("aria-expanded", on ? "true" : "false");
      // three grid cells per item
      const cell = item.closest(".ah-timeline-near-cell, .ah-timeline-far-cell");
      const i = cell && cell.parentNode ? Array.prototype.indexOf.call(cell.parentNode.children, cell) : -1;
      this.fire<TimelineToggle>("ah:toggle", { expanded: on, index: Math.floor(i / 3) });
    });
    this.delegate("keydown", ITEM, (e, item) => { keyClick(e, item); });
  }
}

AH.register("timeline", TimelineController);
