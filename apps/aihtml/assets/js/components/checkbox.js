/* checkbox behaviour (designs/04-components.md): mirrors the native
 * input onto sigil's classes; sigil's three states (checked -> mixed ->
 * unchecked) and locked. Methods never fire change.
 */
import AH from "../core.js";
import "./_lib_choice.js";

var L = AH.lib.choice;

function checkState(input) {
  return input.indeterminate ? "mixed" : input.checked;
}

function setCheck(input, v) {
  var mixed = v === "mixed" || v === "indeterminate" || v === null;
  input.indeterminate = mixed;
  input.checked = !mixed && L.truthy(v);
  L.syncCheckbox(input);
}

AH.register("checkbox", class extends AH.Controller {
  setup() {
    var el = this.element, input = L.inputOf(el);
    if (!input) { return; }
    if (el.classList.contains("ah-checkbox-indeterminate")) {
      input.indeterminate = true;
    }
    this.state = checkState(input);
    this.toggled = false;
    L.bindLocked(this);
    // A click on the input (mouse, label, Space) is what toggles it; a
    // change dispatched by a script does not step the three states.
    this.delegate("click", L.INPUT, (e) => {
      if (!e.defaultPrevented) { this.toggled = true; }
    });
    this.delegate("change", L.INPUT, () => {
      var user = this.toggled;
      this.toggled = false;
      // sigil's three states: checked -> mixed -> unchecked -> checked
      if (user && el.hasAttribute("data-ah-three-states")) {
        var prev = this.state;
        setCheck(input, prev === true ? "mixed" : prev === "mixed" ? false : true);
      }
      this.state = checkState(input);
      L.syncCheckbox(input);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // true | false | "mixed"
  setChecked(v) {
    var input = L.inputOf(this.element);
    setCheck(input, v);
    this.state = checkState(input);
  }
  getValue() { return checkState(L.inputOf(this.element)); }
  setDisabled(on) { L.setDisabled(L.inputOf(this.element), on, L.syncCheckbox); }
});
