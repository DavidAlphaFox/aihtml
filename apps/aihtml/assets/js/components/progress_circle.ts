/* progress-circle behaviour (designs/04-components.md): setValue /
   getValue. "change" and, at 100, "ah:complete" with detail
   ProgressChange {previous, value} (_lib_progress.ts). */
import AH from "../core.ts";
import { clamp, num } from "./_lib_display.ts";
import { fireProgress } from "./_lib_progress.ts";

const CIRC = 2 * Math.PI * 45;

class ProgressCircleController extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setValue(value: unknown): void {
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
    fireProgress(el, old, v, 100);
  }

  getValue(): number { return num(this.element.getAttribute("data-ah-value"), 0); }
}

AH.register("progress-circle", ProgressCircleController);
