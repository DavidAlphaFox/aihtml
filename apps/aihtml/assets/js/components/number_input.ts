/* Behaviour of the number_input component (designs/04-components.md).
 * Ported from sigil: form/number_input. Stepping fires native "input"
 * and "change" on the inner <input>, as typing does. */
import AH from "../core.ts";
import { emit, focusShell, syncLabel } from "./_lib_input.ts";

/** The settings the server renders on the root. */
interface Options {
  min: number | null;
  max: number | null;
  step: number;
  decimals: number;
  allowNull: boolean;
}

/** The spin buttons' repeat: the first delay, then the interval. */
interface Spin {
  delay: number;
  every?: number;
}

function clamp(v: number, o: Options): number {
  if (o.min !== null) { v = Math.max(o.min, v); }
  if (o.max !== null) { v = Math.min(o.max, v); }
  return v;
}

function parseNum(text: unknown): number | null {
  const s = String(text == null ? "" : text).replace(/,/g, "").trim();
  if (s === "") { return null; }
  const n = parseFloat(s);
  return isNaN(n) ? null : n;
}

class NumberInputController extends AH.Controller {
  #spin: Spin | null = null;

  override setup(): void {
    const el = this.element, input = this.field();
    this.#spin = null;
    if (!input) { return; }
    focusShell(this, input, "ah-numinput");
    this.listen(input, "keydown", (e) => {
      const k = e.key || "";
      if (e.ctrlKey || e.metaKey || e.altKey) { return; }
      if (k === "ArrowUp" || k === "ArrowDown") {
        e.preventDefault();
        this.step(k === "ArrowUp" ? 1 : -1);
      } else if (k === "PageUp" || k === "PageDown") {
        e.preventDefault();
        this.step((k === "PageUp" ? 1 : -1) * 10);
      } else if (k === "-") {
        // a minus sign only at the start, once
        if (input.selectionStart !== 0 || input.value.indexOf("-") >= 0) { e.preventDefault(); }
      } else if (k === ".") {
        if (this.opts().decimals === 0 || input.value.indexOf(".") >= 0) { e.preventDefault(); }
      } else if (k.length === 1 && !/[0-9]/.test(k)) {
        e.preventDefault();
      }
    });
    // Typing ends in a native change (on blur or Enter): normalise first,
    // this handler runs before the delegated action handlers.
    this.listen(input, "change", () => {
      this.write(parseNum(input.value));
    });
    this.listen(input, "wheel", (e) => {
      if (document.activeElement !== input) { return; }
      e.preventDefault();
      this.step(e.deltaY < 0 ? 1 : -1);
    }, { passive: false });
    // Spin buttons: step, then repeat after 400ms every 75ms (sigil).
    this.delegate("mousedown", ".ah-numinput-spin-up, .ah-numinput-spin-down", (e, btn) => {
      if (e.button !== 0) { return; }
      e.preventDefault();
      const dir = btn.classList.contains("ah-numinput-spin-up") ? 1 : -1;
      this.stopRepeat();
      this.step(dir);
      const t: Spin = {
        delay: setTimeout(() => {
          t.every = setInterval(() => { this.step(dir); }, 75);
        }, 400)
      };
      this.#spin = t;
      if (document.activeElement !== input) { input.focus(); }
    });
    el.querySelectorAll(".ah-numinput-spin").forEach((s) => {
      this.listen(s, "mouseleave", () => { this.stopRepeat(); });
    });
    this.listen(document, "mouseup", () => { this.stopRepeat(); });
  }

  override teardown(): void { this.stopRepeat(); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): number | null {
    const input = this.field();
    return input ? parseNum(input.value) : null;
  }
  setValue(v: unknown): void {
    const input = this.field();
    if (!input) { return; }
    const before = input.value;
    this.write(v === null || v === undefined ? null : parseNum(v));
    if (input.value !== before) { emit(input, "change"); }
  }
  stepUp(): void { this.step(1); }
  stepDown(): void { this.step(-1); }
  clear(): void {
    const input = this.field();
    if (!input) { return; }
    const before = input.value;
    this.write(null);
    if (input.value !== before) { emit(input, "change"); }
  }
  focus(): void {
    const input = this.field();
    if (input) { input.focus(); }
  }

  private field(): HTMLInputElement | null {
    return this.element.querySelector<HTMLInputElement>("input.ah-numinput-input");
  }

  private opts(): Options {
    const el = this.element;
    const num = (a: string): number | null => {
      const v = el.getAttribute(a);
      return v === null || v === "" ? null : parseFloat(v);
    };
    return {
      min: num("data-min"),
      max: num("data-max"),
      step: num("data-step") || 1,
      decimals: parseInt(el.getAttribute("data-decimals") || "0", 10),
      allowNull: el.getAttribute("data-allow-null") !== "false"
    };
  }

  // Write a (clamped, formatted) value; returns the value written.
  private write(value: number | null): number | null {
    const o = this.opts(), input = this.field();
    const v = value === null ? (o.allowNull ? null : clamp(0, o)) : clamp(value, o);
    if (!input) { return v; }
    input.value = v === null ? "" : v.toFixed(o.decimals);
    if (v === null) { input.removeAttribute("aria-valuenow"); } else { input.setAttribute("aria-valuenow", String(v)); }
    syncLabel(this.element, "ah-numinput", v !== null, document.activeElement === input);
    return v;
  }

  private step(dir: number): void {
    const input = this.field();
    if (!input || input.disabled || input.readOnly) { return; }
    const o = this.opts();
    const before = input.value;
    const cur = parseNum(before);
    const next = parseFloat(((cur === null ? 0 : cur) + o.step * dir).toFixed(o.decimals));
    this.write(next);
    if (input.value !== before) {
      emit(input, "input");
      emit(input, "change");
    }
  }

  private stopRepeat(): void {
    const t = this.#spin;
    if (t) { clearTimeout(t.delay); clearInterval(t.every); }
    this.#spin = null;
  }
}

AH.register("number-input", NumberInputController);
