/* switch-button behaviour (designs/04-components.md): mirrors the native
 * checkbox onto sigil's classes; locked. Methods never fire change.
 */
import AH from "../core.ts";
import { INPUT, bindLocked, inputOf, setDisabled, syncSwitch, truthy } from "./_lib_choice.ts";

class SwitchButtonController extends AH.Controller {
  override setup(): void {
    bindLocked(this);
    this.delegate<Event, HTMLInputElement>("change", INPUT, (_e, input) => { syncSwitch(input); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setChecked(v: unknown): void {
    const input = inputOf(this.element);
    input.checked = truthy(v);
    syncSwitch(input);
  }
  getValue(): boolean { return inputOf(this.element).checked; }
  setDisabled(on: unknown): void { setDisabled(inputOf(this.element), on, syncSwitch); }
}

AH.register("switch-button", SwitchButtonController);
