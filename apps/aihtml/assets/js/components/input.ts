/* Behaviour of the input component, also used by textarea
 * (designs/04-components.md). Ported from sigil: form/input. */
import AH from "../core.ts";
import { emit, focusShell, syncLabel } from "./_lib_input.ts";
import type { Field } from "./_lib_input.ts";

class InputController extends AH.Controller {
  override setup(): void {
    const el = this.element, input = this.field();
    if (!input) { return; }
    focusShell(this, input, "ah-input");
    this.listen(input, "input", () => { this.sync(); });
    this.delegate("click", ".ah-input-clear", (e) => {
      e.preventDefault();
      this.clearField();
      input.focus();
    });
    if (el.classList.contains("ah-input-clearable")) {
      this.listen(input, "keydown", (e) => {
        if (e.key === "Escape" && input.value !== "") {
          e.preventDefault();
          this.clearField();
        }
      });
    }
    this.sync();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string | undefined { const i = this.field(); return i ? i.value : undefined; }
  setValue(v: unknown): void {
    const i = this.field();
    if (!i) { return; }
    i.value = v == null ? "" : String(v);
    this.sync();
  }
  clear(): void { this.clearField(); }
  focus(): void { const i = this.field(); if (i) { i.focus(); } }
  selectAll(): void {
    const i = this.field();
    if (i) { i.focus(); i.select(); }
  }

  private field(): Field | null {
    return this.element.querySelector<Field>("input.ah-input, textarea.ah-input");
  }

  private sync(): void {
    const input = this.field();
    if (!input) { return; }
    const filled = input.value !== "";
    this.element.classList.toggle("ah-input-has-value", filled);
    syncLabel(this.element, "ah-input", filled, document.activeElement === input);
  }

  private clearField(): void {
    const input = this.field();
    if (!input || input.value === "") { return; }
    input.value = "";
    this.sync();
    // Native events, so on(input|change, ...) on the <input> hears them.
    emit(input, "input");
    emit(input, "change");
  }
}

AH.register("input", InputController);
