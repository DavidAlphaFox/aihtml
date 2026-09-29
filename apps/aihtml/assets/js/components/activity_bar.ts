/* Behaviour of the activity bar (designs/04-components.md), ported from
 * sigil's layout/activity_bar. The value (the active item) is kept in
 * data-ah-value on the root, mirrored into a hidden input, and "change"
 * fires when the user changes it; ah:select fires on every user choice,
 * detail ActivitySelect = the item's id. Methods called by the server
 * (AH.invoke / aihtml_action:call) do not fire "change". */
import AH from "../core.ts";
import { setValue } from "./_lib_layout.ts";

/** Detail of ah:select: the chosen item's data-id. */
export type ActivitySelect = string | null;

// ------------------------------------------------------------------
// ActivityBar: a vertical tablist of icon buttons
// ------------------------------------------------------------------

class ActivityBarController extends AH.Controller {
  override setup(): void {
    this.delegate("click", ".ah-activity-bar__item", (_e, item) => {
      this.choose(item);
    });
    // WAI-ARIA tabs: arrows move and activate, Home / End jump
    this.delegate("keydown", ".ah-activity-bar__item", (e, item) => {
      const en = this.enabled();
      const i = en.indexOf(item);
      let next: number;
      switch (e.key) {
        case "ArrowDown": case "ArrowRight": next = (i + 1) % en.length; break;
        case "ArrowUp": case "ArrowLeft": next = (i - 1 + en.length) % en.length; break;
        case "Home": next = 0; break;
        case "End": next = en.length - 1; break;
        default: return;
      }
      e.preventDefault();
      const t = en[next];
      if (t) {
        t.focus();
        this.choose(t);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(v: unknown): void { this.activate(v); }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }

  private items(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(":scope > .ah-activity-bar__item"));
  }

  private enabled(): HTMLElement[] {
    return this.items().filter((it) => it.getAttribute("data-disabled") !== "true");
  }

  private activate(id: unknown): void {
    const items = this.items();
    items.forEach((it) => {
      const on = it.getAttribute("data-id") === String(id);
      it.setAttribute("data-active", String(on));
      it.setAttribute("aria-selected", String(on));
      it.setAttribute("tabindex", on ? "0" : "-1");
    });
    // keep one item reachable with Tab when nothing is active
    if (!items.some((it) => it.getAttribute("tabindex") === "0")) {
      const first = this.enabled()[0];
      if (first) { first.setAttribute("tabindex", "0"); }
    }
    setValue(this.element, id == null ? "" : String(id));
  }

  private choose(item: HTMLElement): void {
    const el = this.element;
    if (item.getAttribute("data-disabled") === "true") { return; }
    const id = item.getAttribute("data-id");
    const changed = el.getAttribute("data-ah-value") !== id;
    this.activate(id);
    this.fire<ActivitySelect>("ah:select", id);
    if (changed) { this.fire("change"); }
  }
}

AH.register("activity-bar", ActivityBarController);
