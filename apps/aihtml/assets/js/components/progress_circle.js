/* progress-circle behaviour (designs/04-components.md): setValue /
   getValue. "change" and, at 100, "ah:complete" with detail
   {previous, value}. */
import AH from "../core.js";
import "./_lib_display.js";
import "./_lib_progress.js";

const { num, clamp } = AH.lib.display;
const CIRC = 2 * Math.PI * 45;

AH.register("progress-circle", class extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setValue(value) {
    const el = this.element;
    const old = num(el.getAttribute("data-ah-value"), 0);
    const v = Math.trunc(clamp(num(value, 0), 0, 100));
    el.classList.remove("ah-progress-circle--indeterminate");
    el.removeAttribute("aria-busy");
    el.querySelectorAll(".ah-progress-circle-fill").forEach((n) => {
      n.setAttribute("stroke-dashoffset", String(CIRC * (1 - v / 100)));
    });
    el.querySelectorAll(".ah-progress-circle-value").forEach((n) => { n.textContent = v + "%"; });
    el.setAttribute("data-ah-value", String(v));
    el.setAttribute("aria-valuenow", String(v));
    el.setAttribute("aria-valuetext", v + "%");
    AH.lib.progress.fire(el, old, v, 100);
  }

  getValue() { return num(this.element.getAttribute("data-ah-value"), 0); }
});
