/* expandable-text behaviour (designs/04-components.md): the toggle swaps
   the cut and the full text; "ah:toggle" with detail true (expanded) or
   false (ExpandableToggle). */
import AH from "../core.ts";

/** Detail of ah:toggle: true when the text is now expanded. */
export type ExpandableToggle = boolean;

class ExpandableTextController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ".ah-expandable-text__toggle", () => { this.toggle(); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  toggle(): void { this.setExpanded(this.element.getAttribute("data-expanded") !== "true"); }
  expand(): void { this.setExpanded(true); }
  collapse(): void { this.setExpanded(false); }

  private setExpanded(on: boolean): void {
    const el = this.element;
    const btn = el.querySelector<HTMLElement>(":scope > .ah-expandable-text__toggle");
    if (!btn || (el.getAttribute("data-expanded") === "true") === on) { return; }
    el.setAttribute("data-expanded", on ? "true" : "false");
    el.querySelectorAll<HTMLElement>("[data-ah-part=short]").forEach((n) => { n.hidden = on; });
    el.querySelectorAll<HTMLElement>("[data-ah-part=full]").forEach((n) => { n.hidden = !on; });
    btn.setAttribute("aria-expanded", on ? "true" : "false");
    btn.textContent = btn.getAttribute(on ? "data-ah-collapse-label" : "data-ah-expand-label");
    this.fire<ExpandableToggle>("ah:toggle", on);
  }
}

AH.register("expandable-text", ExpandableTextController);
