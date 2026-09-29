/* timeline behaviour (designs/04-components.md): cards with a
   description expand on click / Enter; "ah:toggle" with detail
   {expanded, index} (index: the item's position in the timeline). */
import AH from "../core.js";
import "./_lib_display.js";

const ITEM = ".ah-timeline-item[ah-collapsible]";

AH.register("timeline", class extends AH.Controller {
  setup() {
    this.delegate("click", ITEM, (e, item) => {
      const on = item.classList.toggle("ah-timeline-item-expanded");
      item.setAttribute("aria-expanded", on ? "true" : "false");
      // three grid cells per item
      const cell = item.closest(".ah-timeline-near-cell, .ah-timeline-far-cell");
      const i = cell && cell.parentNode ? Array.prototype.indexOf.call(cell.parentNode.children, cell) : -1;
      this.fire("ah:toggle", { expanded: on, index: Math.floor(i / 3) });
    });
    this.delegate("keydown", ITEM, AH.lib.display.keyClick);
  }
});
