/* radiobutton behaviour (designs/04-components.md): restyles every radio
 * of the native group when one changes; locked. Methods never fire change.
 */
import AH from "../core.ts";
import { INPUT, bindLocked, inputOf, setDisabled, syncRadio, truthy } from "./_lib_choice.ts";

// The radios a browser treats as one group with this one.
function sameGroup(input: HTMLInputElement): HTMLInputElement[] {
  if (!input.name) {
    return [input];
  }
  return Array.from((input.form || document).querySelectorAll<HTMLInputElement>("input[type=radio]"))
    .filter((r) => r.name === input.name && r.form === input.form);
}

function syncRadioGroup(input: HTMLInputElement): void {
  sameGroup(input).forEach((r) => { syncRadio(r); });
}

class RadiobuttonController extends AH.Controller {
  override setup(): void {
    bindLocked(this);
    this.delegate<Event, HTMLInputElement>("change", INPUT, (_e, input) => { syncRadioGroup(input); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setChecked(v: unknown): void {
    const input = inputOf(this.element);
    input.checked = truthy(v);
    syncRadioGroup(input);
  }
  getValue(): boolean { return inputOf(this.element).checked; }
  setDisabled(on: unknown): void { setDisabled(inputOf(this.element), on, syncRadio); }
}

AH.register("radiobutton", RadiobuttonController);
