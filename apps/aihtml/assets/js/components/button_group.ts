/* button-group: radio / checkbox selection (arrow keys in radio mode), a
 * short pressed flash in the default mode (designs/04-components.md).
 * In radio and checkbox mode the root keeps data-ah-value and the hidden
 * input in step and fires change (detail ButtonChange: the new value). */
import AH from "../core.ts";
import { setValue, step } from "./_lib_button.ts";
import { join, split } from "./_lib_values.ts";

type Mode = "radio" | "checkbox" | "default";

class ButtonGroupController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate<MouseEvent, HTMLButtonElement>("click", ".ah-btn-group-btn", (_e, btn) => {
      if (btn.disabled || el.classList.contains("ah-btn-group-disabled")) { return; }
      this.clickBtn(btn);
    });
    this.delegate<MouseEvent, HTMLButtonElement>("mouseover", ".ah-btn-group-btn", (_e, btn) => {
      if (!btn.disabled) { btn.classList.add("ah-btn-group-btn-hover"); }
    });
    this.delegate("mouseout", ".ah-btn-group-btn", (e, btn) => {
      if (e.relatedTarget instanceof Node && btn.contains(e.relatedTarget)) { return; }
      btn.classList.remove("ah-btn-group-btn-hover");
    });
    // Radio mode is a radiogroup: arrows move focus and select.
    this.delegate<KeyboardEvent, HTMLButtonElement>("keydown", ".ah-btn-group-btn", (e, btn) => {
      if (this.mode() !== "radio") { return; }
      const btns = this.buttons().filter((b) => !b.disabled);
      const i = step(e.key, btns.indexOf(btn), btns.length);
      if (i < 0) { return; }
      e.preventDefault();
      const to = btns[i];
      if (!to) { return; }
      to.focus();
      this.clickBtn(to);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: unknown): void {
    const vals: unknown[] = Array.isArray(v) ? v
      : v == null || v === "" ? []
      : this.mode() === "radio" ? [String(v)] : split(String(v));
    this.set(vals.filter((x) => x !== ""));
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  clear(): void { this.set([]); }

  private mode(): Mode {
    const c = this.element.classList;
    return c.contains("ah-btn-group-radio") ? "radio"
      : c.contains("ah-btn-group-checkbox") ? "checkbox" : "default";
  }

  private buttons(): HTMLButtonElement[] {
    // the group's buttons are <button>s (aihtml_button_group)
    return Array.from(this.element.children)
      .filter((b): b is HTMLButtonElement => b.classList.contains("ah-btn-group-btn"));
  }

  private select(btn: HTMLButtonElement, on: boolean): void {
    btn.classList.toggle("ah-btn-group-btn-selected", on);
    if (this.mode() === "radio") {
      btn.setAttribute("aria-checked", String(on));
      btn.setAttribute("tabindex", on ? "0" : "-1");
    } else {
      btn.setAttribute("aria-pressed", String(on));
    }
  }

  private sync(fire: boolean): void {
    const vals = this.buttons().filter((b) => b.classList.contains("ah-btn-group-btn-selected"))
      .map((b) => b.getAttribute("data-value"));
    // radio: the value itself; checkbox: _lib_values text
    setValue(this.element, this.mode() === "radio" ? (vals[0] || "") : join(vals), fire);
  }

  private set(values: unknown[]): void {
    const set = new Set(values.map(String));
    this.buttons().forEach((b) => { this.select(b, set.has(String(b.getAttribute("data-value")))); });
    if (this.mode() === "radio" && !this.buttons().some((b) => b.getAttribute("tabindex") === "0")) {
      const first = this.buttons().find((b) => !b.disabled);
      if (first) { first.setAttribute("tabindex", "0"); }
    }
    this.sync(false);
  }

  private clickBtn(btn: HTMLButtonElement): void {
    switch (this.mode()) {
      case "radio":
        this.buttons().forEach((b) => { this.select(b, b === btn); });
        this.sync(true);
        break;
      case "checkbox":
        this.select(btn, !btn.classList.contains("ah-btn-group-btn-selected"));
        this.sync(true);
        break;
      default:
        btn.classList.add("ah-btn-group-btn-pressed");
        setTimeout(() => { btn.classList.remove("ah-btn-group-btn-pressed"); }, 150);
    }
  }
}

AH.register("button-group", ButtonGroupController);
