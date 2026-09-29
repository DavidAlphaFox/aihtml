/* progressbar behaviour (designs/04-components.md): setValue / getValue.
   "change" and, at max, "ah:complete" with detail ProgressChange
   {previous, value} (_lib_progress.ts). */
import AH from "../core.ts";
import { clamp, num } from "./_lib_display.ts";
import { fireProgress, pct } from "./_lib_progress.ts";

class ProgressbarController extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setValue(value: unknown, text?: unknown): void {
    const el = this.element;
    const lo = num(el.getAttribute("data-ah-min"), 0), hi = num(el.getAttribute("data-ah-max"), 100);
    const old = num(el.getAttribute("data-ah-value"), lo);
    const v = clamp(num(value, lo), lo, hi);
    const p = pct(v, lo, hi);
    const dim = el.classList.contains("ah-progressbar-vertical") ? "height" : "width";
    el.classList.remove("ah-progressbar-indeterminate");
    el.removeAttribute("aria-busy");
    el.querySelectorAll<HTMLElement>(":scope > .ah-progressbar-value, :scope > .ah-progressbar-value-vertical")
      .forEach((n) => { n.style[dim] = p + "%"; });
    el.querySelectorAll<HTMLElement>(":scope > .ah-progressbar-range").forEach((n) => {
      const stop = num(n.getAttribute("data-ah-stop"), hi);
      n.style[dim] = pct(Math.min(stop, hi, v), lo, hi) + "%";
    });
    const label = text !== undefined && text !== null ? String(text)
      : (el.getAttribute("data-ah-text") === "custom" ? null : Math.round(p) + "%");
    if (label !== null) {
      el.querySelectorAll(".ah-progressbar-text").forEach((n) => { n.textContent = label; });
    }
    el.setAttribute("data-ah-value", String(v));
    el.setAttribute("aria-valuenow", String(v));
    el.setAttribute("aria-valuetext", label !== null ? label : Math.round(p) + "%");
    fireProgress(el, old, v, hi);
  }

  getValue(): number { return num(this.element.getAttribute("data-ah-value"), 0); }
}

AH.register("progressbar", ProgressbarController);
