/* Behaviour of the colorpicker (designs/04-components.md). Ported from
 * sigil: form/colorpicker (+ colorpicker/color, events, render).
 *
 * Value-bearing: data-ah-value and the hidden input follow the value, the
 * root fires `change` on commit and `input` while dragging (detail of
 * both ColorpickerValue: {value}). Native input/change events of the
 * inner text fields are stopped at the root so they are not taken for
 * the component's own events. */
import AH from "../core.ts";
import { PickerPopup, clamp, disabled, drag, fenceNativeEvents, pointerXY, round2, uid } from "./_lib_picker.ts";

/** Detail of `input` and `change`: the colour, "#rrggbb" ("#rrggbbaa"
 *  with alpha below 255), or "" when cleared. */
export interface ColorpickerValue { value: string; }

interface Rgb { r: number; g: number; b: number; }
interface Rgba extends Rgb { a: number; }
interface Hsv { h: number; s: number; v: number; }

/** Which text inputs a sync leaves alone (the one being typed in). */
type Skip = "hex" | "rgb" | undefined;

const CP = "ah-colorpicker";

function hsvToRgb(h: number, s0: number, v0: number): Rgb {
  const s = s0 / 100, v = v0 / 100;
  const c = v * s;
  const x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  const m = v - c;
  const rgb = h < 60 ? [c, x, 0] : h < 120 ? [x, c, 0] : h < 180 ? [0, c, x]
    : h < 240 ? [0, x, c] : h < 300 ? [x, 0, c] : [c, 0, x];
  return { r: Math.round((rgb[0] + m) * 255), g: Math.round((rgb[1] + m) * 255),
           b: Math.round((rgb[2] + m) * 255) };
}

function rgbToHsv(r0: number, g0: number, b0: number): Hsv {
  const r = r0 / 255, g = g0 / 255, b = b0 / 255;
  const max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
  const h = d === 0 ? 0 : max === r ? 60 * ((((g - b) / d) % 6 + 6) % 6)
    : max === g ? 60 * ((b - r) / d + 2) : 60 * ((r - g) / d + 4);
  return { h: Math.round(h) % 360, s: Math.round(max === 0 ? 0 : d / max * 100),
           v: Math.round(max * 100) };
}

function hex2(n: number): string { return (n < 16 ? "0" : "") + n.toString(16); }

/** "#rgb", "rgb", "#rrggbb", with alpha also 4 and 8 digits -> {r,g,b,a} */
function parseHex(s: string | null | undefined, alpha: boolean): Rgba | null {
  let h = String(s || "").trim().replace(/^#/, "");
  if (!/^[0-9a-f]+$/i.test(h)) { return null; }
  if (h.length === 3 || (alpha && h.length === 4)) {
    h = h.replace(/./g, "$&$&");
  }
  if (h.length !== 6 && !(alpha && h.length === 8)) { return null; }
  return { r: parseInt(h.slice(0, 2), 16), g: parseInt(h.slice(2, 4), 16),
           b: parseInt(h.slice(4, 6), 16), a: h.length === 8 ? parseInt(h.slice(6, 8), 16) : 255 };
}

class ColorpickerController extends AH.Controller {
  #h = 0;
  #s = 100;
  #v = 100;
  #a = 255;
  #alpha = false;
  #committed = "";
  // The exact RGB a colour was loaded with (hex, RGB inputs, swatch) is
  // kept until the HSV controls change it, so rounding through HSV does
  // not alter a typed colour.
  #exact: (Hsv & { rgb: Rgb }) | null = null;
  #trigger: HTMLElement | null = null;
  #map: HTMLElement | null = null;
  #popup = new PickerPopup(this, CP);

  override setup(): void {
    const el = this.element;
    const q = (sel: string): HTMLElement | null => el.querySelector<HTMLElement>(sel);
    const value = el.getAttribute("data-ah-value") || "";
    const alpha = this.#alpha = el.getAttribute("data-alpha") === "true";
    this.#h = 0;
    this.#s = 100;
    this.#v = 100;
    this.#a = 255;
    this.#committed = value;
    this.#exact = null;
    const start = parseHex(value, alpha);
    if (start) { this.load(start); }
    const trigger = this.#trigger = q(":scope > ." + CP + "-trigger");
    const popupEl = this.#popup.popup;
    if (popupEl) {
      popupEl.id = popupEl.id || uid("ah-cp-popup-");
      if (trigger) { trigger.setAttribute("aria-controls", popupEl.id); }
    }
    fenceNativeEvents(this);
    const map = this.#map = q("." + CP + "-map");

    if (trigger) {
      this.listen(trigger, "click", (e) => {
        e.preventDefault();
        if (this.#popup.isOpen) { this.closePopup(true); } else { this.openPopup(); }
      });
      this.listen(trigger, "keydown", (e) => {
        if (e.key === "ArrowDown") { e.preventDefault(); this.openPopup(); }
      });
    }
    this.listen(el, "keydown", (e) => {
      if (e.key === "Escape" && this.#popup.isOpen) {
        e.preventDefault();
        e.stopPropagation();
        this.closePopup(true);
      }
    });

    // Saturation/value area (sigil 处理面板拖拽).
    const fromMap = (e: PointerEvent): void => {
      if (!map) { return; }
      const r = map.getBoundingClientRect();
      const p = pointerXY(e);
      this.#s = Math.round(clamp((p.x - r.left) / r.width, 0, 1) * 100);
      this.#v = Math.round((1 - clamp((p.y - r.top) / r.height, 0, 1)) * 100);
      this.emit("input");
    };
    const endDrag = (): void => { this.emit("change"); };
    drag(this, map, fromMap, fromMap, endDrag);

    // Hue bar (sigil 处理色相拖拽) and alpha bar.
    const hue = q("." + CP + "-bar:not(." + CP + "-alpha)");
    const fromHue = (e: PointerEvent): void => {
      if (!hue) { return; }
      const r = hue.getBoundingClientRect();
      this.#h = Math.round(clamp((pointerXY(e).y - r.top) / r.height, 0, 1) * 360) % 360;
      this.emit("input");
    };
    drag(this, hue, fromHue, fromHue, endDrag);
    const alphaBar = q("." + CP + "-alpha");
    const fromAlpha = (e: PointerEvent): void => {
      if (!alphaBar) { return; }
      const r = alphaBar.getBoundingClientRect();
      this.#a = Math.round((1 - clamp((pointerXY(e).y - r.top) / r.height, 0, 1)) * 255);
      this.emit("input");
    };
    drag(this, alphaBar, fromAlpha, fromAlpha, endDrag);

    // Keyboard: arrows move by 1, Shift by 10; Home / End.
    this.keys(map, (key, n) => {
      switch (key) {
        case "ArrowLeft": this.#s = clamp(this.#s - n, 0, 100); break;
        case "ArrowRight": this.#s = clamp(this.#s + n, 0, 100); break;
        case "ArrowUp": this.#v = clamp(this.#v + n, 0, 100); break;
        case "ArrowDown": this.#v = clamp(this.#v - n, 0, 100); break;
        case "Home": this.#s = 0; break;
        case "End": this.#s = 100; break;
        default: return false;
      }
      return true;
    });
    // The hue grows downwards on the bar, so Down increases it.
    this.keys(hue, (key, n) => {
      switch (key) {
        case "ArrowDown": case "ArrowRight": this.#h = (this.#h + n) % 360; break;
        case "ArrowUp": case "ArrowLeft": this.#h = (this.#h - n + 360) % 360; break;
        case "Home": this.#h = 0; break;
        case "End": this.#h = 359; break;
        default: return false;
      }
      return true;
    });
    this.keys(alphaBar, (key, n) => {
      const step = Math.round(n * 2.55);
      switch (key) {
        case "ArrowUp": case "ArrowRight": this.#a = clamp(this.#a + step, 0, 255); break;
        case "ArrowDown": case "ArrowLeft": this.#a = clamp(this.#a - step, 0, 255); break;
        case "Home": this.#a = 0; break;
        case "End": this.#a = 255; break;
        default: return false;
      }
      return true;
    });

    // Hex input (sigil 处理hex输入): live while valid, commit on change.
    const hexIn = el.querySelector<HTMLInputElement>("." + CP + "-hex-input");
    if (hexIn) {
      this.listen(hexIn, "input", () => {
        const col = parseHex(hexIn.value, alpha);
        const len = String(hexIn.value).trim().replace(/^#/, "").length;
        if (col && len >= 6) {
          this.load(col);
          this.emit("input", "hex");
        }
      });
      this.listen(hexIn, "change", () => {
        const col = parseHex(hexIn.value, alpha);
        if (col) { this.load(col); }
        this.emit("change");
      });
      this.listen(hexIn, "keydown", (e) => {
        if (e.key === "Enter") { e.preventDefault(); hexIn.dispatchEvent(new Event("change", { bubbles: true })); }
      });
    }

    // RGB(A) inputs (sigil 处理rgb输入).
    el.querySelectorAll<HTMLInputElement>(
      "." + CP + "-r-input, ." + CP + "-g-input, ." + CP + "-b-input, ." + CP + "-a-input")
      .forEach((i) => {
        this.listen(i, "input", () => {
          if (this.fromRgb()) { this.emit("input", "rgb"); }
        });
        this.listen(i, "change", () => {
          this.fromRgb();
          this.emit("change");
        });
      });

    // Swatches and the clear link (sigil's transparent link).
    this.delegate<MouseEvent, HTMLElement>("click", "." + CP + "-swatch", (e, sw) => {
      e.preventDefault();
      const col = parseHex(sw.getAttribute("data-color"), alpha);
      if (!col || disabled(el)) { return; }
      this.load(col);
      this.emit("input");
      this.emit("change");
    });
    this.delegate<MouseEvent, HTMLElement>("click", "." + CP + "-transparent a", (e) => {
      e.preventDefault();
      if (disabled(el)) { return; }
      this.clearValue();
      this.closePopup(true);
    });
    this.sync();
  }

  override teardown(): void { this.#popup.stop(); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string { return this.element.getAttribute("data-ah-value") || ""; }
  // Set the value without firing events (server driven); "" clears.
  setValue(v: unknown): void {
    const col = parseHex(v == null ? "" : String(v), this.#alpha);
    if (!col) {
      this.#committed = "";
      this.show("");
      return;
    }
    this.load(col);
    this.sync();
    this.#committed = this.hex();
    this.show(this.#committed);
  }
  clear(): void { this.clearValue(); }
  open(): void { this.openPopup(); }
  close(): void { this.closePopup(false); }

  // ---- popup ----

  private openPopup(): void {
    this.#popup.open(this.#trigger, () => { this.sync(); });
    if (this.#map) { this.#map.focus(); }
  }

  private closePopup(refocus: boolean): void { this.#popup.close(this.#trigger, refocus); }

  // Arrow keys on a control: apply(key, step) returns false for other
  // keys; then input and change fire.
  private keys(t: HTMLElement | null, apply: (key: string, n: number) => boolean): void {
    if (!t) { return; }
    this.listen(t, "keydown", (e) => {
      if (disabled(this.element)) { return; }
      const n = e.shiftKey ? 10 : 1;
      if (apply(e.key, n)) {
        e.preventDefault();
        this.emit("input");
        this.emit("change");
      }
    });
  }

  private fromRgb(): boolean {
    const el = this.element;
    const n = (cls: string, max: number): number | null => {
      const i = el.querySelector<HTMLInputElement>("." + CP + "-" + cls + "-input");
      const v = parseInt(i ? i.value : "", 10);
      return isNaN(v) ? null : clamp(v, 0, max);
    };
    const r = n("r", 255), g = n("g", 255), b = n("b", 255);
    if (r === null || g === null || b === null) { return false; }
    const a = this.#alpha ? n("a", 100) : 100;
    this.load({ r, g, b, a: a === null ? this.#a : Math.round(a * 2.55) });
    return true;
  }

  // ---- colour ----

  private rgb(): Rgb {
    const e = this.#exact;
    if (e && e.h === this.#h && e.s === this.#s && e.v === this.#v) { return e.rgb; }
    return hsvToRgb(this.#h, this.#s, this.#v);
  }

  private hex(): string {
    const c = this.rgb();
    const hex = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
    return this.#alpha && this.#a < 255 ? hex + hex2(this.#a) : hex;
  }

  private rgba(): string {
    const c = this.rgb();
    return "rgba(" + c.r + "," + c.g + "," + c.b + "," + round2(this.#a / 255) + ")";
  }

  private load(c: Rgba): void {
    const hsv = rgbToHsv(c.r, c.g, c.b);
    // Keep the hue when the colour is grey or black, so the area does
    // not jump back to red.
    if (hsv.s === 0 || hsv.v === 0) { hsv.h = this.#h; }
    if (hsv.v === 0) { hsv.s = this.#s; }
    this.#h = hsv.h; this.#s = hsv.s; this.#v = hsv.v; this.#a = c.a;
    this.#exact = { h: this.#h, s: this.#s, v: this.#v, rgb: { r: c.r, g: c.g, b: c.b } };
  }

  // sigil 同步全部UI!: area colour, pointers, preview, inputs; plus the
  // alpha bar, swatches, ARIA and the popup trigger.
  private sync(skip?: Skip): void {
    const el = this.element;
    const h = this.#h, s = this.#s, v = this.#v;
    const rgb = this.rgb();
    const bright = 0.299 * rgb.r + 0.587 * rgb.g + 0.114 * rgb.b > 150;
    const hex6 = "#" + hex2(rgb.r) + hex2(rgb.g) + hex2(rgb.b);
    const all = <E extends Element = HTMLElement>(sel: string, f: (x: E) => void): void => {
      el.querySelectorAll<E>(sel).forEach(f);
    };
    all("." + CP + "-map", (map) => {
      map.style.backgroundColor = "hsl(" + h + ", 100%, 50%)";
      map.setAttribute("aria-valuenow", String(s));
      map.setAttribute("aria-valuetext", "Saturation " + s + "%, brightness " + v + "%");
      map.querySelectorAll<HTMLElement>("." + CP + "-map-pointer").forEach((p) => {
        p.style.left = s + "%";
        p.style.top = (100 - v) + "%";
        p.classList.toggle(CP + "-map-pointer-dark", bright);
        p.classList.toggle(CP + "-map-pointer-light", !bright);
      });
    });
    all("." + CP + "-bar:not(." + CP + "-alpha)", (hue) => {
      hue.setAttribute("aria-valuenow", String(h));
      hue.querySelectorAll<HTMLElement>("." + CP + "-bar-pointer").forEach((p) => {
        p.style.top = (h / 360 * 100) + "%";
      });
    });
    const pct = Math.round(this.#a / 255 * 100);
    all("." + CP + "-alpha", (a) => {
      a.style.setProperty("--ah-cp-rgb", hex6);
      a.setAttribute("aria-valuenow", String(pct));
      a.setAttribute("aria-valuetext", pct + "%");
      a.querySelectorAll<HTMLElement>("." + CP + "-bar-pointer").forEach((p) => { p.style.top = (100 - pct) + "%"; });
    });
    all("." + CP + "-preview", (p) => { p.style.backgroundColor = this.rgba(); });
    if (skip !== "hex") {
      all<HTMLInputElement>("." + CP + "-hex-input", (i) => { i.value = this.hex().slice(1); });
    }
    if (skip !== "rgb") {
      all<HTMLInputElement>("." + CP + "-r-input", (i) => { i.value = String(rgb.r); });
      all<HTMLInputElement>("." + CP + "-g-input", (i) => { i.value = String(rgb.g); });
      all<HTMLInputElement>("." + CP + "-b-input", (i) => { i.value = String(rgb.b); });
      all<HTMLInputElement>("." + CP + "-a-input", (i) => { i.value = String(pct); });
    }
  }

  // ---- value ----

  // The value-bearing side: data-ah-value, hidden input, trigger, swatches.
  private show(value: string): void {
    const el = this.element;
    el.setAttribute("data-ah-value", value);
    const hidden = el.querySelector<HTMLInputElement>(":scope > input[type=hidden]");
    if (hidden) { hidden.value = value; }
    const trigger = el.querySelector<HTMLElement>(":scope > ." + CP + "-trigger");
    if (trigger) {
      trigger.querySelectorAll<HTMLElement>("." + CP + "-trigger-swatch").forEach((s) => {
        s.classList.toggle(CP + "-trigger-empty", value === "");
        if (value === "") { s.style.removeProperty("--ah-cp-swatch"); } else { s.style.setProperty("--ah-cp-swatch", this.rgba()); }
      });
      trigger.querySelectorAll("." + CP + "-trigger-text").forEach((t) => {
        t.textContent = value === "" ? (trigger.getAttribute("data-placeholder") || "") : value;
      });
    }
    el.querySelectorAll("." + CP + "-swatch").forEach((s) => {
      s.setAttribute("aria-pressed", String(s.getAttribute("data-color") === value));
    });
  }

  // type: "input" (live) or "change" (commit). change fires only when the
  // value differs from the last committed one.
  private emit(type: "input" | "change", skip?: Skip): void {
    this.sync(skip);
    const value = this.hex();
    const before = this.element.getAttribute("data-ah-value");
    this.show(value);
    if (type === "input") {
      if (value !== before) { this.fire<ColorpickerValue>("input", { value }); }
    } else {
      this.commit(value);
    }
  }

  private commit(value: string): void {
    if (value !== this.#committed) {
      this.#committed = value;
      this.fire<ColorpickerValue>("change", { value });
    }
  }

  private clearValue(): void {
    this.show("");
    this.commit("");
  }
}

AH.register("colorpicker", ColorpickerController);
