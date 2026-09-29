/* button-group: radio / checkbox selection (arrow keys in radio mode), a
 * short pressed flash in the default mode (designs/04-components.md).
 * In radio and checkbox mode the root keeps data-ah-value and the hidden
 * input in step and fires change (detail: the new value). */
import AH from "../core.js";
import "./_lib_button.js";
import "./_lib_values.js";

var L = AH.lib.button;

AH.register("button-group", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.delegate("click", ".ah-btn-group-btn", (e, btn) => {
      if (btn.disabled || el.classList.contains("ah-btn-group-disabled")) { return; }
      this.clickBtn(btn);
    });
    this.delegate("mouseover", ".ah-btn-group-btn", (e, btn) => {
      if (!btn.disabled) { btn.classList.add("ah-btn-group-btn-hover"); }
    });
    this.delegate("mouseout", ".ah-btn-group-btn", (e, btn) => {
      if (e.relatedTarget && btn.contains(e.relatedTarget)) { return; }
      btn.classList.remove("ah-btn-group-btn-hover");
    });
    // Radio mode is a radiogroup: arrows move focus and select.
    this.delegate("keydown", ".ah-btn-group-btn", (e, btn) => {
      if (this.mode() !== "radio") { return; }
      var btns = this.buttons().filter((b) => !b.disabled);
      var i = L.step(e.key, btns.indexOf(btn), btns.length);
      if (i < 0) { return; }
      e.preventDefault();
      var to = btns[i];
      to.focus();
      this.clickBtn(to);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) {
    var vals = Array.isArray(v) ? v
      : v == null || v === "" ? []
      : this.mode() === "radio" ? [String(v)] : AH.lib.values.split(v);
    this.set(vals.filter((x) => x !== ""));
  }
  getValue() { return this.element.getAttribute("data-ah-value"); }
  clear() { this.set([]); }

  mode() {
    var c = this.element.classList;
    return c.contains("ah-btn-group-radio") ? "radio"
      : c.contains("ah-btn-group-checkbox") ? "checkbox" : "default";
  }

  buttons() {
    return Array.from(this.element.children).filter((b) => b.classList.contains("ah-btn-group-btn"));
  }

  select(btn, on) {
    btn.classList.toggle("ah-btn-group-btn-selected", on);
    if (this.mode() === "radio") {
      btn.setAttribute("aria-checked", String(on));
      btn.setAttribute("tabindex", on ? "0" : "-1");
    } else {
      btn.setAttribute("aria-pressed", String(on));
    }
  }

  sync(fire) {
    var vals = this.buttons().filter((b) => b.classList.contains("ah-btn-group-btn-selected"))
      .map((b) => b.getAttribute("data-value"));
    // radio: the value itself; checkbox: AH.lib.values text
    L.setValue(this.element, this.mode() === "radio" ? (vals[0] || "") : AH.lib.values.join(vals), fire);
  }

  set(values) {
    var set = {};
    values.forEach((v) => { set[String(v)] = true; });
    this.buttons().forEach((b) => { this.select(b, !!set[b.getAttribute("data-value")]); });
    if (this.mode() === "radio" && !this.buttons().some((b) => b.getAttribute("tabindex") === "0")) {
      var first = this.buttons().find((b) => !b.disabled);
      if (first) { first.setAttribute("tabindex", "0"); }
    }
    this.sync(false);
  }

  clickBtn(btn) {
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
});
