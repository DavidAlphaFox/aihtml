/* toggle-button: a click toggles aria-pressed / data-ah-value and fires
 * change on the root (detail ButtonChange: "true" | "false")
 * (designs/04-components.md). */
import AH from "../core.ts";
import { setValue } from "./_lib_button.ts";

class ToggleButtonController extends AH.Controller {
  override setup(): void {
    this.listen(this.element, "click", () => {
      if ((this.element as HTMLElement & { disabled?: boolean }).disabled) { return; }
      this.press(!this.pressed(), true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  toggle(): void { this.press(!this.pressed(), false); }
  setValue(v: unknown): void { this.press(v === true || v === "true", false); }
  getValue(): boolean { return this.pressed(); }

  private pressed(): boolean { return this.element.getAttribute("aria-pressed") === "true"; }

  private press(on: boolean, fire: boolean): void {
    const el = this.element, v = on ? "true" : "false";
    el.classList.toggle("ah-btn-toggled", on);
    el.setAttribute("aria-pressed", v);
    if ("value" in el) { el.value = v; }
    setValue(el, v, fire);
  }
}

AH.register("toggle-button", ToggleButtonController);
