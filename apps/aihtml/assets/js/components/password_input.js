/* Behaviour of the password_input component (designs/04-components.md).
 * Ported from sigil: form/password_input. */
import AH from "../core.js";
import "./_lib_input.js";

var L = AH.lib.input;

var SPECIALS = "<>@!#$%^&*()_+[]{}?:;|'\"\\,./~`-=";

// sigil password-input.strength/evaluate
function strength(pw) {
  if (pw.length < 8) { return "too-short"; }
  var letters = 0, numbers = 0, specials = 0;
  for (var i = 0; i < pw.length; i++) {
    var c = pw.charCodeAt(i), ch = pw.charAt(i);
    if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
        (c >= 128 && c <= 154) || (c >= 160 && c <= 165)) { letters++; }
    else if (c >= 48 && c <= 57) { numbers++; }
    else if (SPECIALS.indexOf(ch) >= 0) { specials++; }
  }
  var score = letters + numbers + 2 * specials + letters * numbers / 2 + pw.length;
  return score < 20 ? "weak" : score < 30 ? "fair" : score < 40 ? "good" : "strong";
}

var STRENGTH = {
  "too-short": ["Too short", "20%", "var(--ah-color-error)"],
  weak: ["Weak", "40%", "var(--ah-color-error)"],
  fair: ["Fair", "60%", "var(--ah-color-warning)"],
  good: ["Good", "80%", "var(--ah-color-info)"],
  strong: ["Strong", "100%", "var(--ah-color-success)"]
};

AH.register("password-input", class extends AH.Controller {
  setup() {
    var input = this.field();
    if (!input) { return; }
    L.focusShell(this, input, "ah-pwd");
    this.listen(input, "input", () => {
      this.updateStrength(input.value);
      L.syncLabel(this.element, "ah-pwd", input.value !== "", true);
    });
    // mousedown: keep the focus (and caret) in the field
    this.delegate("mousedown", ".ah-pwd-toggle", (e) => { e.preventDefault(); });
    this.delegate("click", ".ah-pwd-toggle", (e) => {
      e.preventDefault();
      this.setVisible(!this.element.classList.contains("ah-pwd-visible"));
    });
    this.updateStrength(input.value);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.field().value; }
  setValue(v) {
    var input = this.field();
    input.value = v == null ? "" : String(v);
    this.updateStrength(input.value);
    L.syncLabel(this.element, "ah-pwd", input.value !== "", false);
  }
  toggle(show) {
    this.setVisible(show === undefined ? !this.element.classList.contains("ah-pwd-visible") : !!show);
  }
  focus() { this.field().focus(); }

  field() { return this.element.querySelector("input.ah-pwd"); }

  updateStrength(pw) {
    var el = this.element;
    var fill = el.querySelector(".ah-pwd-strength-fill");
    var texts = el.querySelectorAll(".ah-pwd-strength-text");
    if (!fill) { return; }
    var fills = el.querySelectorAll(".ah-pwd-strength-fill");
    if (!pw) {
      fills.forEach((f) => { f.style.width = "0"; f.style.backgroundColor = "transparent"; });
      texts.forEach((t) => { t.textContent = ""; });
      el.removeAttribute("data-strength");
      return;
    }
    var level = strength(pw), d = STRENGTH[level];
    fills.forEach((f) => { f.style.width = d[1]; f.style.backgroundColor = d[2]; });
    texts.forEach((t) => { t.textContent = d[0]; });
    el.setAttribute("data-strength", level);
  }

  setVisible(show) {
    var el = this.element, input = this.field();
    el.classList.toggle("ah-pwd-visible", show);
    if (input) { input.setAttribute("type", show ? "text" : "password"); }
    el.querySelectorAll(".ah-pwd-toggle").forEach((b) => {
      b.setAttribute("aria-pressed", show ? "true" : "false");
      b.setAttribute("aria-label", show ? "Hide password" : "Show password");
    });
  }
});
