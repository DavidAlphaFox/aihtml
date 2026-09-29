/* segmented-control: single selection, arrow keys move and select; the
 * root keeps data-ah-value and the hidden input in step and fires change
 * (detail ButtonChange: the new value) (designs/04-components.md). */
import AH from "../core.ts";
import { setValue, step } from "./_lib_button.ts";

type Item = HTMLElement & { disabled?: boolean };

class SegmentedControlController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate<MouseEvent, Item>("click", ".ah-segmented-control__item", (_e, it) => {
      if (it.disabled || el.getAttribute("data-disabled") === "true"
          || it.getAttribute("data-disabled") === "true") { return; }
      this.set(it.getAttribute("data-value"), true);
    });
    this.delegate<KeyboardEvent, Item>("keydown", ".ah-segmented-control__item", (e, it) => {
      const items = this.items().filter((i) => !i.disabled);
      const i = step(e.key, items.indexOf(it), items.length);
      if (i < 0) { return; }
      e.preventDefault();
      const to = items[i];
      if (!to) { return; }
      to.focus();
      this.set(to.getAttribute("data-value"), true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: unknown): void { this.set(v, false); }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }

  private items(): Item[] {
    return Array.from(this.element.children)
      .filter((i): i is Item => i instanceof HTMLElement && i.classList.contains("ah-segmented-control__item"));
  }

  private set(value: unknown, fire: boolean): void {
    const v = String(value == null ? "" : value);
    let found = false;
    this.items().forEach((it) => {
      const on = it.getAttribute("data-value") === v;
      found = found || on;
      it.setAttribute("data-state", on ? "active" : "inactive");
      it.setAttribute("aria-selected", String(on));
      it.setAttribute("tabindex", on ? "0" : "-1");
    });
    if (!found) {
      const first = this.items().find((i) => !i.disabled);
      if (first) { first.setAttribute("tabindex", "0"); }
    }
    setValue(this.element, v, fire);
  }
}

AH.register("segmented-control", SegmentedControlController);
