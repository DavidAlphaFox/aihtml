/* Behaviour of the password_input component (designs/04-components.md).
 * Ported from sigil: form/password_input. */
import AH from "../core.ts";
import { focusShell, syncLabel } from "./_lib_input.ts";

type Strength = "too-short" | "weak" | "fair" | "good" | "strong";

const SPECIALS = "<>@!#$%^&*()_+[]{}?:;|'\"\\,./~`-=";

// sigil password-input.strength/evaluate
function strength(pw: string): Strength {
  if (pw.length < 8) { return "too-short"; }
  let letters = 0, numbers = 0, specials = 0;
  for (let i = 0; i < pw.length; i++) {
    const c = pw.charCodeAt(i), ch = pw.charAt(i);
    if ((c >= 65 && c <= 90) || (c >= 97 && c <= 122) ||
        (c >= 128 && c <= 154) || (c >= 160 && c <= 165)) { letters++; }
    else if (c >= 48 && c <= 57) { numbers++; }
    else if (SPECIALS.indexOf(ch) >= 0) { specials++; }
  }
  const score = letters + numbers + 2 * specials + letters * numbers / 2 + pw.length;
  return score < 20 ? "weak" : score < 30 ? "fair" : score < 40 ? "good" : "strong";
}

// label (in the page's language), meter width, meter colour
const STRENGTH: Record<Strength, [() => string, string, string]> = {
  "too-short": [() => AH.t("password_input", "too_short", "Too short"), "20%", "var(--ah-color-error)"],
  weak: [() => AH.t("password_input", "weak", "Weak"), "40%", "var(--ah-color-error)"],
  fair: [() => AH.t("password_input", "fair", "Fair"), "60%", "var(--ah-color-warning)"],
  good: [() => AH.t("password_input", "good", "Good"), "80%", "var(--ah-color-info)"],
  strong: [() => AH.t("password_input", "strong", "Strong"), "100%", "var(--ah-color-success)"]
};

class PasswordInputController extends AH.Controller {
  override setup(): void {
    const input = this.field();
    if (!input) { return; }
    focusShell(this, input, "ah-pwd");
    this.listen(input, "input", () => {
      this.updateStrength(input.value);
      syncLabel(this.element, "ah-pwd", input.value !== "", true);
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
  getValue(): string | undefined {
    const input = this.field();
    return input ? input.value : undefined;
  }
  setValue(v: unknown): void {
    const input = this.field();
    if (!input) { return; }
    input.value = v == null ? "" : String(v);
    this.updateStrength(input.value);
    syncLabel(this.element, "ah-pwd", input.value !== "", false);
  }
  /** Show (true), hide (false) or flip (no argument) the password. */
  toggle(show?: boolean): void {
    this.setVisible(show === undefined ? !this.element.classList.contains("ah-pwd-visible") : !!show);
  }
  focus(): void {
    const input = this.field();
    if (input) { input.focus(); }
  }

  private field(): HTMLInputElement | null { return this.element.querySelector<HTMLInputElement>("input.ah-pwd"); }

  private updateStrength(pw: string): void {
    const el = this.element;
    const fill = el.querySelector(".ah-pwd-strength-fill");
    const texts = el.querySelectorAll(".ah-pwd-strength-text");
    if (!fill) { return; }
    const fills = el.querySelectorAll<HTMLElement>(".ah-pwd-strength-fill");
    if (!pw) {
      fills.forEach((f) => { f.style.width = "0"; f.style.backgroundColor = "transparent"; });
      texts.forEach((t) => { t.textContent = ""; });
      el.removeAttribute("data-strength");
      return;
    }
    const level = strength(pw), d = STRENGTH[level];
    fills.forEach((f) => { f.style.width = d[1]; f.style.backgroundColor = d[2]; });
    const text = d[0]();
    texts.forEach((t) => { t.textContent = text; });
    el.setAttribute("data-strength", level);
  }

  private setVisible(show: boolean): void {
    const el = this.element, input = this.field();
    el.classList.toggle("ah-pwd-visible", show);
    if (input) { input.setAttribute("type", show ? "text" : "password"); }
    el.querySelectorAll(".ah-pwd-toggle").forEach((b) => {
      b.setAttribute("aria-pressed", show ? "true" : "false");
      b.setAttribute("aria-label", show ? AH.t("common", "hide_password", "Hide password")
                                       : AH.t("common", "show_password", "Show password"));
    });
  }
}

AH.register("password-input", PasswordInputController);
