/* switch-button behaviour (designs/04-components.md): mirrors the native
 * checkbox onto sigil's classes; locked. Methods never fire change.
 */
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

AH.register("switch-button", class extends AH.Controller {
  setup() {
    L.bindLocked(this);
    this.delegate("change", L.INPUT, (e, input) => { L.syncSwitch(input); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setChecked(v) {
    var input = L.inputOf(this.element);
    input.checked = L.truthy(v);
    L.syncSwitch(input);
  }
  getValue() { return L.inputOf(this.element).checked; }
  setDisabled(on) { L.setDisabled(L.inputOf(this.element), on, L.syncSwitch); }
});
