/* Behaviour of the input_otp component (designs/04-components.md).
 * Ported from sigil: form/input_otp. The root fires "change" (no detail)
 * and, when every slot is filled, "ah:complete" (detail: the code,
 * OtpComplete). */
import AH from "../core.ts";
import { commitValue, isolate } from "./_lib_input.ts";

/** Detail of ah:complete: the whole code. */
export type OtpComplete = string;

const SLOT = ".ah-input-otp__slot";

function index(slot: Element): number { return parseInt(slot.getAttribute("data-index") || "", 10); }

class InputOtpController extends AH.Controller {
  override setup(): void {
    this.delegate<Event, HTMLInputElement>("input", SLOT, (_e, slot) => {
      const len = this.slots().length;
      const i = index(slot);
      const v = this.sanitize(slot.value, len - i);
      if (v.length > 1) {
        // autofill or IME delivered several characters: spread them
        const cur = this.read();
        this.write((cur.slice(0, i) + v).slice(0, len));
        this.commit();
        this.focusAt(i + v.length);
        return;
      }
      slot.value = v;
      this.commit();
      if (v) { this.focusAt(i + 1); }
    });
    // after the handler above: the slots' events stop at the root
    isolate(this, SLOT);
    this.delegate<KeyboardEvent, HTMLInputElement>("keydown", SLOT, (e, slot) => {
      const i = index(slot);
      const n = this.slots().length;
      switch (e.key) {
        case "Backspace":
          e.preventDefault();
          if (slot.value) {
            slot.value = "";                  // clear this one only
            this.commit();
          } else if (i > 0) {
            this.slots()[i - 1].value = "";   // back up and clear
            this.commit();
            this.focusAt(i - 1);
          }
          break;
        case "Delete":
          e.preventDefault();
          slot.value = "";
          this.commit();
          break;
        case "ArrowLeft": e.preventDefault(); this.focusAt(i - 1); break;
        case "ArrowRight": e.preventDefault(); this.focusAt(i + 1); break;
        case "Home": e.preventDefault(); this.focusAt(0); break;
        case "End": e.preventDefault(); this.focusAt(n - 1); break;
        default:
          // typing over a filled slot replaces it
          if (e.key && e.key.length === 1 && !e.ctrlKey && !e.metaKey &&
              slot.value && slot.selectionStart === slot.selectionEnd) {
            slot.select();
          }
      }
    });
    this.delegate("paste", SLOT, (e) => {
      e.preventDefault();
      const cd = e.clipboardData;
      const n = this.slots().length;
      const v = this.sanitize(cd ? cd.getData("text") : "", n);
      if (!v) { return; }
      this.write(v);
      this.commit();
      this.focusAt(Math.min(v.length, n - 1));
    });
    this.listen(this.element, "focusin", (e) => {
      const t = e.target;
      if (t instanceof HTMLInputElement && t.classList.contains("ah-input-otp__slot")) { t.select(); }
    });
    // A click on an empty slot past the first gap goes to the gap.
    this.delegate<MouseEvent, HTMLInputElement>("mousedown", SLOT, (e, slot) => {
      const gap = this.read().length;
      const i = index(slot);
      if (!slot.value && i > gap) {
        e.preventDefault();
        this.focusAt(gap);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v: unknown): void {
    this.write(this.sanitize(v, this.slots().length));
    this.commit();
  }
  clear(): void { this.write(""); this.commit(); }
  focus(): void { this.focusAt(this.read().length); }
  /** Mark the code wrong (e.g. after the server rejected it); false clears. */
  invalid(on?: boolean): void {
    if (on === false) { this.element.removeAttribute("data-invalid"); }
    else { this.element.setAttribute("data-invalid", "true"); }
  }

  private slots(): HTMLInputElement[] {
    return Array.from(this.element.querySelectorAll<HTMLInputElement>(SLOT));
  }

  private sanitize(s: unknown, len: number): string {
    const re = this.element.getAttribute("data-pattern") === "alphanumeric" ? /[^0-9a-zA-Z]/g : /[^0-9]/g;
    return String(s || "").replace(re, "").slice(0, Math.max(0, len));
  }

  private read(): string { return this.slots().map((s) => s.value).join(""); }

  private focusAt(i: number): void {
    const s = this.slots();
    if (!s.length) { return; }
    const slot = s[Math.max(0, Math.min(i, s.length - 1))];
    slot.focus();
    slot.select();
  }

  // Lay a value out over the slots (from the first).
  private write(v: string): void {
    this.slots().forEach((s, i) => { s.value = v.charAt(i); });
  }

  private commit(): void {
    const el = this.element, s = this.slots();
    s.forEach((x) => { x.setAttribute("data-filled", x.value ? "true" : "false"); });
    // The value is the filled prefix: a gap ends it.
    let v = "";
    for (let i = 0; i < s.length && s[i].value; i++) { v += s[i].value; }
    const complete = v.length === s.length;
    el.setAttribute("data-complete", complete ? "true" : "false");
    el.removeAttribute("data-invalid");
    if (commitValue(el, v) && complete) {
      this.fire<OtpComplete>("ah:complete", v);
    }
  }
}

AH.register("input-otp", InputOtpController);
