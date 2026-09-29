/* badge behaviour (designs/04-components.md): setCount(n) with the same
   max / zero rules as the server. */
import AH from "../core.ts";
import { num } from "./_lib_display.ts";

class BadgeController extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  /** a number (or numeric text) with the max / zero rules, or any text */
  setCount(n: unknown): void {
    const el = this.element;
    const ind = el.querySelector(":scope > .ah-badge-indicator");
    if (!ind || ind.getAttribute("data-dot") === "true") { return; }
    const max = num(el.getAttribute("data-ah-max"), 99);
    const isNum = typeof n === "number" || (n !== "" && n !== null && !isNaN(Number(n)));
    const v = isNum ? Number(n) : n;
    ind.textContent = v === null || v === undefined ? "" : (isNum && Number(v) > max ? max + "+" : String(v));
    const hide = isNum && v === 0 && el.getAttribute("data-ah-show-zero") !== "true";
    ind.setAttribute("data-invisible", hide ? "true" : "false");
  }
}

AH.register("badge", BadgeController);
