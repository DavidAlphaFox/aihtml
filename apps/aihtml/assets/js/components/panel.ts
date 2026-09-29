/* Behaviour of panel: a scroll area with an optional collapsible header.
 * A user toggle fires ah:expand / ah:collapse (no detail) on the root
 * when the slide ends. */
import AH from "../core.ts";
import { slide } from "./_lib_layout.ts";

class PanelController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate("click", ".ah-panel-toggle", (e, t) => {
      if (t.closest(".ah-panel") !== el) {
        return;
      }
      e.preventDefault();
      this.set(t.getAttribute("aria-expanded") !== "true", true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  scrollTo(x?: number, y?: number): void {
    const w = this.element.querySelector<HTMLElement>(":scope > .ah-panel-wrapper");
    if (w) {
      w.scrollLeft = x || 0;
      w.scrollTop = y || 0;
    }
  }
  refresh(): void { /* native scrolling: nothing to measure */ }
  collapse(): void { this.set(false, false); }
  expand(): void { this.set(true, false); }
  toggle(): void { this.set(this.element.classList.contains("ah-panel-collapsed"), false); }

  private set(open: boolean, user: boolean): void {
    const el = this.element;
    const w = el.querySelector<HTMLElement>(":scope > .ah-panel-wrapper");
    const t = el.querySelector(":scope > .ah-panel-header > .ah-panel-toggle");
    if (!t || (t.getAttribute("aria-expanded") === "true") === open) {
      return;
    }
    t.setAttribute("aria-expanded", String(open));
    el.classList.toggle("ah-panel-collapsed", !open);
    const done = (): void => {
      if (user) { this.fire(open ? "ah:expand" : "ah:collapse"); }
    };
    if (w) { slide(w, open, 200, done); } else { done(); }
  }
}

AH.register("panel", PanelController);
