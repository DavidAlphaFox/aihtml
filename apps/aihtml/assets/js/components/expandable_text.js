/* expandable-text behaviour (designs/04-components.md): the toggle swaps
   the cut and the full text; "ah:toggle" with detail true (expanded) or
   false. */
import AH from "../core.js";

AH.register("expandable-text", class extends AH.Controller {
  setup() {
    this.delegate("click", ".ah-expandable-text__toggle", () => { this.toggle(); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  toggle() { this.setExpanded(this.element.getAttribute("data-expanded") !== "true"); }
  expand() { this.setExpanded(true); }
  collapse() { this.setExpanded(false); }

  setExpanded(on) {
    const el = this.element;
    const btn = el.querySelector(":scope > .ah-expandable-text__toggle");
    if (!btn || (el.getAttribute("data-expanded") === "true") === on) { return; }
    el.setAttribute("data-expanded", on ? "true" : "false");
    el.querySelectorAll("[data-ah-part=short]").forEach((n) => { n.hidden = on; });
    el.querySelectorAll("[data-ah-part=full]").forEach((n) => { n.hidden = !on; });
    btn.setAttribute("aria-expanded", on ? "true" : "false");
    btn.textContent = btn.getAttribute(on ? "data-ah-collapse-label" : "data-ah-expand-label");
    this.fire("ah:toggle", on);
  }
});
