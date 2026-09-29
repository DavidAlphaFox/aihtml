/* progressbar behaviour (designs/04-components.md): setValue / getValue.
   "change" and, at max, "ah:complete" with detail {previous, value}. */
import AH from "../core.js";
import "./_lib_display.js";
import "./_lib_progress.js";

const { num, clamp } = AH.lib.display;
const P = AH.lib.progress;

AH.register("progressbar", class extends AH.Controller {
  // methods (aihtml_action:call/4, AH.invoke)
  setValue(value, text) {
    const el = this.element;
    const lo = num(el.getAttribute("data-ah-min"), 0), hi = num(el.getAttribute("data-ah-max"), 100);
    const old = num(el.getAttribute("data-ah-value"), lo);
    const v = clamp(num(value, lo), lo, hi);
    const p = P.pct(v, lo, hi);
    const dim = el.classList.contains("ah-progressbar-vertical") ? "height" : "width";
    el.classList.remove("ah-progressbar-indeterminate");
    el.removeAttribute("aria-busy");
    el.querySelectorAll(":scope > .ah-progressbar-value, :scope > .ah-progressbar-value-vertical")
      .forEach((n) => { n.style[dim] = p + "%"; });
    el.querySelectorAll(":scope > .ah-progressbar-range").forEach((n) => {
      const stop = num(n.getAttribute("data-ah-stop"), hi);
      n.style[dim] = P.pct(Math.min(stop, hi, v), lo, hi) + "%";
    });
    const label = text !== undefined && text !== null ? String(text)
      : (el.getAttribute("data-ah-text") === "custom" ? null : Math.round(p) + "%");
    if (label !== null) {
      el.querySelectorAll(".ah-progressbar-text").forEach((n) => { n.textContent = label; });
    }
    el.setAttribute("data-ah-value", String(v));
    el.setAttribute("aria-valuenow", String(v));
    el.setAttribute("aria-valuetext", label !== null ? label : Math.round(p) + "%");
    P.fire(el, old, v, hi);
  }

  getValue() { return num(this.element.getAttribute("data-ah-value"), 0); }
});
