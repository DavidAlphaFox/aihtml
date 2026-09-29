/* badge behaviour (designs/04-components.md): setCount(n) with the same
   max / zero rules as the server. */
import AH from "../core.js";
import "./_lib_display.js";

const num = AH.lib.display.num;

AH.register("badge", class extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setCount(n) {
    const el = this.element;
    const ind = el.querySelector(":scope > .ah-badge-indicator");
    if (!ind || ind.getAttribute("data-dot") === "true") { return; }
    const max = num(el.getAttribute("data-ah-max"), 99);
    const isNum = typeof n === "number" || (n !== "" && n !== null && !isNaN(n));
    const v = isNum ? Number(n) : n;
    ind.textContent = v === null || v === undefined ? "" : (isNum && v > max ? max + "+" : String(v));
    const hide = isNum && v === 0 && el.getAttribute("data-ah-show-zero") !== "true";
    ind.setAttribute("data-invisible", hide ? "true" : "false");
  }
});
