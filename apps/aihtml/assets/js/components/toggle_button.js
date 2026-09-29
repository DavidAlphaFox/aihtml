/* toggle-button: a click toggles aria-pressed / data-ah-value and fires
 * change on the root (detail: "true" | "false") (designs/04-components.md). */
import AH from "../core.js";
import "./_lib_button.js";

var L = AH.lib.button;

AH.register("toggle-button", class extends AH.Controller {
  setup() {
    this.listen(this.element, "click", () => {
      if (this.element.disabled) { return; }
      this.press(!this.pressed(), true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  toggle() { this.press(!this.pressed(), false); }
  setValue(v) { this.press(v === true || v === "true", false); }
  getValue() { return this.pressed(); }

  pressed() { return this.element.getAttribute("aria-pressed") === "true"; }

  press(on, fire) {
    var el = this.element, v = on ? "true" : "false";
    el.classList.toggle("ah-btn-toggled", on);
    el.setAttribute("aria-pressed", v);
    if ("value" in el) { el.value = v; }
    L.setValue(el, v, fire);
  }
});
