/* Behaviour of formatted_input (designs/04-components.md), after sigil's
 * form/formatted_input.
 *
 *   formatted-input  an integer (BigInt) typed in radix 2/8/10/16; arrow
 *                    keys and the spin buttons (held: repeat) step it; a
 *                    radix menu (AH.float); data-ah-value stays decimal
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change" (detail: the decimal value, FormattedValue),
 * "ah:open" / "ah:close" (no detail) and "ah:radix-change" (detail:
 * RadixChange {radix, old}).
 *
 * The server renders the whole first state, so setup only binds events.
 */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";

/** Detail of input / change: the decimal value. */
export type FormattedValue = string;
/** Detail of ah:radix-change. */
export interface RadixChange { radix: Radix; old: Radix; }

type Radix = 2 | 8 | 10 | 16;

// ------------------------------------------------------------------
// formatted-input
// ------------------------------------------------------------------

const RADIX_PREFIX: Record<Radix, string> = { 2: "0b", 8: "0o", 10: "", 16: "0x" };
const RADIX_CHARS: Record<Radix, RegExp> = { 2: /^[01]$/, 8: /^[0-7]$/, 10: /^[0-9]$/, 16: /^[0-9a-f]$/i };

function asRadix(n: number): Radix | null {
  return n === 2 || n === 8 || n === 10 || n === 16 ? n : null;
}

// Text in a radix -> BigInt; empty or invalid -> 0n.
function bigParse(raw: unknown, radix: Radix): bigint {
  let s = String(raw === null || raw === undefined ? "" : raw).trim();
  if (!s) { return BigInt(0); }
  const neg = s.charAt(0) === "-";
  if (neg) { s = s.slice(1); }
  if (!s) { s = "0"; }
  try {
    const b = BigInt(RADIX_PREFIX[radix] + s);
    return neg ? -b : b;
  } catch (_e) {
    return BigInt(0);
  }
}

function bigText(b: bigint, radix: Radix, upper: boolean, expo: boolean): string {
  let s = b.toString(radix);
  if (upper) { s = s.toUpperCase(); }
  if (expo && radix === 10) {
    const neg = s.charAt(0) === "-", abs = neg ? s.slice(1) : s;
    if (abs.length > 1) { s = (neg ? "-" : "") + abs.charAt(0) + "." + abs.slice(1) + "e+" + (abs.length - 1); }
  }
  return s;
}

function writeValue(el: Element, v: string): void {
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = v; });
}

class FormattedInputController extends AH.Controller {
  #input: HTMLInputElement | null = null;
  #popup: HTMLElement | null = null;
  #btn: HTMLElement | null = null;
  #radix: Radix = 10;
  #min: bigint | null = null;
  #max: bigint | null = null;
  #step = BigInt(1);
  #upper = false;
  #expo = false;
  #value = BigInt(0);
  #open = false;
  #editing = false;
  #focusValue: string | null = null;
  #timer: ReturnType<typeof setTimeout> | null = null;
  #iv: ReturnType<typeof setInterval> | null = null;
  #float: FloatHandle | null = null;
  #spinUp: AbortController | null = null;

  override setup(): void {
    const el = this.element;
    const min = el.getAttribute("data-ah-min"), max = el.getAttribute("data-ah-max");
    this.#input = el.querySelector<HTMLInputElement>("input.ah-fmt-input");
    this.#popup = el.querySelector<HTMLElement>(":scope > .ah-fmt-popup");
    this.#btn = el.querySelector<HTMLElement>(".ah-fmt-dropdown-btn");
    this.#radix = asRadix(parseInt(el.getAttribute("data-ah-radix") ?? "", 10)) ?? 10;
    this.#min = min === null ? null : BigInt(min);
    this.#max = max === null ? null : BigInt(max);
    this.#step = BigInt(el.getAttribute("data-ah-step") || "1");
    this.#upper = el.hasAttribute("data-ah-upper");
    this.#expo = el.getAttribute("data-ah-notation") === "exponential";
    this.#value = BigInt(el.getAttribute("data-ah-value") || "0");
    this.#open = false;
    this.#editing = false;
    this.#focusValue = null;
    const inp = this.#input;
    if (!inp) { return; }

    this.listen(inp, "keydown", (e) => {
      const k = e.key || "";
      if (this.#open && (k === "ArrowDown" || k === "ArrowUp")) {
        e.preventDefault();
        const items = this.items();
        let idx = items.findIndex((i) => i.classList.contains("ah-fmt-popup-item-hover"));
        idx = (idx + (k === "ArrowDown" ? 1 : -1) + items.length) % items.length;
        this.activate(items[idx]);
        return;
      }
      if (this.#open && (k === "Enter" || k === " ")) {
        e.preventDefault();
        const hover = this.items().find((i) => i.classList.contains("ah-fmt-popup-item-hover"));
        this.applyRadix(hover ? hover.getAttribute("data-radix") : undefined);
        return;
      }
      if (k === "Escape") {
        if (this.#open) { e.preventDefault(); this.closeMenu(); }
        return;
      }
      if (e.altKey && (k === "ArrowDown" || k === "ArrowUp")) {
        e.preventDefault();
        if (k === "ArrowDown") { this.openMenu(); } else { this.closeMenu(); }
        return;
      }
      if (e.ctrlKey || e.metaKey || e.altKey) { return; }
      if (k === "ArrowUp" || k === "ArrowDown") {
        e.preventDefault();
        this.set(bigParse(inp.value, this.#radix), false);   // what was typed so far
        this.stepBy(k === "ArrowUp" ? 1 : -1);
        return;
      }
      if (k === "-") {
        if (inp.selectionStart !== 0 || inp.value.charAt(0) === "-" && inp.selectionEnd === 0) {
          e.preventDefault();
        }
        return;
      }
      if (k.length === 1 && !RADIX_CHARS[this.#radix].test(k)) { e.preventDefault(); }
    });
    this.listen(inp, "input", () => {
      const b = bigParse(inp.value, this.#radix), v = b.toString();
      this.#value = b;
      if (v !== el.getAttribute("data-ah-value")) {
        writeValue(el, v);
        this.fire<FormattedValue>("input", v);
      }
    });
    this.listen(inp, "focus", () => {
      el.classList.add("ah-fmt-input-focused");
      this.#editing = true;
      this.#focusValue = el.getAttribute("data-ah-value");
      if (this.#expo) { this.show(); }
    });
    this.listen(inp, "blur", () => {
      el.classList.remove("ah-fmt-input-focused");
      this.#editing = false;
      const before = this.#focusValue;
      this.#focusValue = null;
      this.set(bigParse(inp.value, this.#radix), false);
      const v = el.getAttribute("data-ah-value");
      if (before !== null && v !== before) { this.fire("change", v); }
    });

    this.delegate("mousedown", ".ah-fmt-spin-up, .ah-fmt-spin-down", (e, spin) => {
      if (e.button !== 0 || inp.disabled) { return; }
      e.preventDefault();
      const dir = spin.classList.contains("ah-fmt-spin-up") ? 1 : -1;
      if (this.#editing) { this.set(bigParse(inp.value, this.#radix), false); }
      this.startRepeat(() => { this.stepBy(dir); }, 400, 75);
      if (!this.#spinUp) {
        this.#spinUp = new AbortController();
        const stop = (): void => {
          this.stopRepeat();
          if (this.#spinUp) { this.#spinUp.abort(); this.#spinUp = null; }
        };
        document.addEventListener("mouseup", stop, { signal: this.#spinUp.signal });
      }
    });
    this.delegate("mousedown", ".ah-fmt-dropdown-btn", (e) => {
      e.preventDefault();
      if (this.#open) { this.closeMenu(); } else { this.openMenu(); }
    });
    if (this.#popup) {
      // the popup floats (AH.float) but stays inside the root
      this.delegate("mousedown", ".ah-fmt-popup-item", (e, item) => {
        e.preventDefault();
        this.applyRadix(item.getAttribute("data-radix"));
      }, this.#popup);
    }
    this.listen(document, "mousedown", (e) => {
      if (this.#open && !(e.target instanceof Node && el.contains(e.target))) { this.closeMenu(); }
    });
  }

  override teardown(): void {
    this.stopRepeat();
    if (this.#spinUp) { this.#spinUp.abort(); this.#spinUp = null; }
    if (this.#float) { this.#float.stop(); this.#float = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v: unknown): void {
    this.set(bigParse(String(v), 10), false);
    if (this.#editing) { this.#focusValue = this.element.getAttribute("data-ah-value"); }
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  setRadix(radix: number | string): void { this.applyRadix(radix); }
  getRadix(): Radix { return this.#radix; }
  open(): void { this.openMenu(); }
  close(): void { this.closeMenu(); }

  private clamp(b: bigint): bigint {
    if (this.#min !== null && b < this.#min) { return this.#min; }
    if (this.#max !== null && b > this.#max) { return this.#max; }
    return b;
  }

  // Show the value in the radix (plain while the field has focus).
  private show(): void {
    const inp = this.#input;
    if (!inp) { return; }
    const editing = document.activeElement === inp;
    const t = bigText(this.#value, this.#radix, this.#upper, this.#expo && !editing);
    inp.value = t;
    inp.setAttribute("aria-valuenow", this.#value.toString());
    inp.setAttribute("aria-valuetext", t);
  }

  // Set the value (clamped); fire change when asked and it changed.
  private set(b: bigint, fire: boolean): void {
    const el = this.element, old = el.getAttribute("data-ah-value");
    this.#value = this.clamp(b);
    this.show();
    const v = this.#value.toString();
    writeValue(el, v);
    if (fire && v !== old) { this.fire<FormattedValue>("change", v); }
  }

  private stepBy(dir: number): void {
    if (!this.#input || this.#input.disabled) { return; }
    this.set(this.#value + this.#step * BigInt(dir), true);
    if (this.#editing) { this.#focusValue = this.element.getAttribute("data-ah-value"); }
  }

  private items(): HTMLElement[] {
    return this.#popup
      ? Array.from(this.#popup.children).filter((i): i is HTMLElement => i.classList.contains("ah-fmt-popup-item"))
      : [];
  }

  private activate(item: HTMLElement | undefined): void {
    this.items().forEach((i) => { i.classList.remove("ah-fmt-popup-item-hover"); });
    if (!item) { this.#input?.removeAttribute("aria-activedescendant"); return; }
    item.classList.add("ah-fmt-popup-item-hover");
    this.#input?.setAttribute("aria-activedescendant", item.id);
  }

  private openMenu(): void {
    const el = this.element;
    if (this.#open || !this.#popup || !this.#input || this.#input.disabled) { return; }
    this.#open = true;
    this.#popup.classList.add("ah-fmt-popup-open");
    // the input row is always rendered around the input
    const row = el.querySelector(":scope > .ah-fmt-input-row")!;
    this.#float = AH.float(this.#popup, row, { placement: "bottom", align: "end" });
    if (this.#btn) { this.#btn.setAttribute("aria-expanded", "true"); }
    this.activate(this.items().find((i) => i.classList.contains("ah-fmt-popup-item-active")));
    this.fire("ah:open");
  }

  private closeMenu(): void {
    if (!this.#open) { return; }
    this.#open = false;
    this.#popup?.classList.remove("ah-fmt-popup-open");
    if (this.#float) { this.#float.stop(); this.#float = null; }
    if (this.#btn) { this.#btn.setAttribute("aria-expanded", "false"); }
    this.#input?.removeAttribute("aria-activedescendant");
    this.items().forEach((i) => { i.classList.remove("ah-fmt-popup-item-hover"); });
    this.fire("ah:close");
  }

  private applyRadix(raw: unknown): void {
    const old = this.#radix;
    const radix = asRadix(parseInt(String(raw), 10));
    if (radix === null) { return; }
    this.closeMenu();
    if (radix === old) { return; }
    this.#radix = radix;
    this.element.setAttribute("data-ah-radix", String(radix));
    this.items().forEach((i) => {
      const on = i.getAttribute("data-radix") === String(radix);
      i.classList.toggle("ah-fmt-popup-item-active", on);
      i.setAttribute("aria-selected", String(on));
    });
    this.show();
    this.fire<RadixChange>("ah:radix-change", { radix: radix, old: old });
  }

  private stopRepeat(): void {
    if (this.#timer) { clearTimeout(this.#timer); this.#timer = null; }
    if (this.#iv) { clearInterval(this.#iv); this.#iv = null; }
  }

  private startRepeat(f: () => void, delay: number, interval: number): void {
    this.stopRepeat();
    f();
    this.#timer = setTimeout(() => {
      this.#timer = null;
      this.#iv = setInterval(f, interval);
    }, delay);
  }
}

AH.register("formatted-input", FormattedInputController);
