/* radiobutton behaviour (designs/04-components.md): restyles every radio
 * of the native group when one changes; locked. Methods never fire change.
 */
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

// The radios a browser treats as one group with this one.
function sameGroup(input) {
  if (!input.name) {
    return [input];
  }
  return Array.from((input.form || document).querySelectorAll("input[type=radio]")).filter(function (r) {
    return r.name === input.name && r.form === input.form;
  });
}

function syncRadioGroup(input) {
  sameGroup(input).forEach(function (r) { L.syncRadio(r); });
}

AH.register("radiobutton", class extends AH.Controller {
  setup() {
    L.bindLocked(this);
    this.delegate("change", L.INPUT, (e, input) => { syncRadioGroup(input); });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setChecked(v) {
    var input = L.inputOf(this.element);
    input.checked = L.truthy(v);
    syncRadioGroup(input);
  }
  getValue() { return L.inputOf(this.element).checked; }
  setDisabled(on) { L.setDisabled(L.inputOf(this.element), on, L.syncRadio); }
});
