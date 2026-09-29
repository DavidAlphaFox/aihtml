/* checkbox behaviour (designs/04-components.md): mirrors the native
 * input onto sigil's classes; sigil's three states (checked -> mixed ->
 * unchecked) and locked. Methods never fire change.
 */
import AH from "../core.ts";
import { INPUT, bindLocked, inputOf, setDisabled, syncCheckbox, truthy } from "./_lib_choice.ts";

/** A checkbox's state: checked, unchecked or mixed (indeterminate). */
export type CheckState = boolean | "mixed";

function checkState(input: HTMLInputElement): CheckState {
  return input.indeterminate ? "mixed" : input.checked;
}

function setCheck(input: HTMLInputElement, v: unknown): void {
  const mixed = v === "mixed" || v === "indeterminate" || v === null;
  input.indeterminate = mixed;
  input.checked = !mixed && truthy(v);
  syncCheckbox(input);
}

class CheckboxController extends AH.Controller {
  #state: CheckState = false;
  #toggled = false;

  override setup(): void {
    const el = this.element, input = el.querySelector<HTMLInputElement>(INPUT);
    if (!input) { return; }
    if (el.classList.contains("ah-checkbox-indeterminate")) {
      input.indeterminate = true;
    }
    this.#state = checkState(input);
    this.#toggled = false;
    bindLocked(this);
    // A click on the input (mouse, label, Space) is what toggles it; a
    // change dispatched by a script does not step the three states.
    this.delegate("click", INPUT, (e) => {
      if (!e.defaultPrevented) { this.#toggled = true; }
    });
    this.delegate("change", INPUT, () => {
      const user = this.#toggled;
      this.#toggled = false;
      // sigil's three states: checked -> mixed -> unchecked -> checked
      if (user && el.hasAttribute("data-ah-three-states")) {
        const prev = this.#state;
        setCheck(input, prev === true ? "mixed" : prev === "mixed" ? false : true);
      }
      this.#state = checkState(input);
      syncCheckbox(input);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  /** true | false | "mixed" */
  setChecked(v: unknown): void {
    const input = inputOf(this.element);
    setCheck(input, v);
    this.#state = checkState(input);
  }
  getValue(): CheckState { return checkState(inputOf(this.element)); }
  setDisabled(on: unknown): void { setDisabled(inputOf(this.element), on, syncCheckbox); }
}

AH.register("checkbox", CheckboxController);
