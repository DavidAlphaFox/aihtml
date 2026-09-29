/* Behaviour of the input component, also used by textarea
 * (designs/04-components.md). Ported from sigil: form/input. */
import AH from "../core.js";
import "./_lib_input.js";

var L = AH.lib.input;

AH.register("input", class extends AH.Controller {
  setup() {
    var el = this.element, input = this.field();
    if (!input) { return; }
    L.focusShell(this, input, "ah-input");
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
  getValue() { var i = this.field(); return i ? i.value : undefined; }
  setValue(v) {
    this.field().value = v == null ? "" : String(v);
    this.sync();
  }
  clear() { this.clearField(); }
  focus() { var i = this.field(); if (i) { i.focus(); } }
  selectAll() {
    var i = this.field();
    if (i) { i.focus(); i.select(); }
  }

  field() { return this.element.querySelector("input.ah-input, textarea.ah-input"); }

  sync() {
    var input = this.field(), filled = input.value !== "";
    this.element.classList.toggle("ah-input-has-value", filled);
    L.syncLabel(this.element, "ah-input", filled, document.activeElement === input);
  }

  clearField() {
    var input = this.field();
    if (!input || input.value === "") { return; }
    input.value = "";
    this.sync();
    // Native events, so on(input|change, ...) on the <input> hears them.
    L.emit(input, "input");
    L.emit(input, "change");
  }
});
