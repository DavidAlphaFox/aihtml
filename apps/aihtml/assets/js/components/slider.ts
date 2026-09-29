/* Behaviour of slider (designs/04-components.md). Ported from sigil
   (sigil.components.form.slider). The root keeps data-ah-value and the
   hidden input in step and fires "input" / "change" (native events, no
   detail). */
import AH from "../core.ts";

// ------------------------------------------------------------------
// slider
// ------------------------------------------------------------------
//
// Positions are written as calc() fractions of the track (as the server
// renders them), so nothing needs re-measuring on resize. Keyboard, per
// sigil: Left/Down decrease, Right/Up increase, Home/End; plus
// PageUp/PageDown by ten steps. Buttons step like sigil's; the wheel
// steps while the slider has focus.

/** One thumb of a range slider; a single slider has only "end". */
type Thumb = "start" | "end";

/** The settings the server renders on the root. */
interface Conf {
  min: number;
  max: number;
  step: number;
  decimals: number;
  minRange: number;
  vertical: boolean;
  range: boolean;
}

const THUMB = 18;

function frac(r: number): string {
  return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + ")";
}

function fracCenter(r: number): string {
  return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + " + " + THUMB / 2 + "px)";
}

function css(node: HTMLElement | null, props: Partial<Record<"left" | "bottom" | "width" | "height", string>>): void {
  if (node) { Object.assign(node.style, props); }
}

function thumbOf(el: Element | null): Thumb {
  return el && el.classList.contains("ah-slider-thumb-start") ? "start" : "end";
}

class SliderController extends AH.Controller {
  #conf: Conf | null = null;
  #dragging = false;
  #thumb: Thumb | undefined = undefined;
  #startValue: string | null = null;

  override setup(): void {
    const el = this.element, c = this.conf();
    this.#dragging = false; this.#thumb = undefined; this.#startValue = null;

    this.delegate("pointerdown", ".ah-slider-content", (e) => {
      if (this.disabled() || e.button !== 0) { return; }
      e.preventDefault();
      const thumb = (e.target as Element).closest(".ah-slider-thumb");
      const v = this.fromPointer(e);
      let which: Thumb = "end";
      if (c.range) {
        if (thumb) {
          which = thumbOf(thumb);
        } else {
          const vals = this.values();
          which = Math.abs(v - vals[0]) <= Math.abs(v - vals[1]) ? "start" : "end";
        }
      }
      this.#dragging = true;
      this.#thumb = which;
      this.#startValue = el.getAttribute("data-ah-value");
      const t = el.querySelector<HTMLElement>(".ah-slider-thumb-" + which);
      if (t) { t.classList.add("ah-slider-thumb-dragging"); }
      const target = c.range ? t : el;
      if (target) { target.focus({ preventScroll: true }); }
      this.showTip(true);
      try { el.setPointerCapture(e.pointerId); } catch { /* synthetic event */ }
      // Pressing the track jumps the nearest thumb there.
      if (!thumb && this.set(which, v)) { this.fire("input"); }
      this.tooltip();
    });
    this.listen(el, "pointermove", (e) => {
      if (!this.#dragging) { return; }
      if (this.set(this.#thumb, this.fromPointer(e))) { this.fire("input"); }
    });
    const up = (e: PointerEvent): void => {
      if (!this.#dragging) { return; }
      this.#dragging = false;
      el.querySelectorAll(".ah-slider-thumb").forEach((t) => { t.classList.remove("ah-slider-thumb-dragging"); });
      try { el.releasePointerCapture(e.pointerId); } catch { /* not captured */ }
      if (!el.contains(document.activeElement)) { this.showTip(false); }
      if (el.getAttribute("data-ah-value") !== this.#startValue) { this.fire("change"); }
    };
    this.listen(el, "pointerup", up);
    this.listen(el, "pointercancel", up);

    this.delegate("click", ".ah-slider-button", (_e, btn) => {
      if (this.disabled()) { return; }
      const inc = btn.classList.contains("ah-slider-button-next");
      // sigil: in range mode "+" moves the end thumb, "-" the start one
      this.stepBy(c.range ? (inc ? "end" : "start") : "end", inc ? c.step : -c.step);
    });

    this.listen(el, "keydown", (e) => {
      if (this.disabled()) { return; }
      let which: Thumb = "end";
      if (c.range) {
        const t = (e.target as Element).closest(".ah-slider-thumb");
        if (!t) { return; }
        which = thumbOf(t);
      }
      this.#thumb = which;
      const big = c.step * Math.max(1, Math.round((c.max - c.min) / c.step / 10));
      const deltas: Record<string, number> = { ArrowRight: c.step, ArrowUp: c.step, ArrowLeft: -c.step,
                                               ArrowDown: -c.step, PageUp: big, PageDown: -big };
      const delta = Object.prototype.hasOwnProperty.call(deltas, e.key) ? deltas[e.key] : undefined;
      if (delta !== undefined) {
        e.preventDefault();
        this.showTip(true);
        this.stepBy(which, delta);
      } else if (e.key === "Home" || e.key === "End") {
        e.preventDefault();
        this.showTip(true);
        if (this.set(which, e.key === "Home" ? c.min : c.max)) {
          this.fire("input");
          this.fire("change");
        }
      }
    });

    this.listen(el, "wheel", (e) => {
      if (this.disabled() || !el.contains(document.activeElement)) { return; }
      e.preventDefault();
      const which: Thumb = c.range && thumbOf(document.activeElement) === "start" ? "start" : "end";
      this.stepBy(which, e.deltaY < 0 ? c.step : -c.step);
    }, { passive: false });

    this.listen(el, "focusin", (e) => {
      el.classList.add("ah-slider-focused");
      if (c.range) { this.#thumb = thumbOf(e.target as Element); }
      this.tooltip();
    });
    this.listen(el, "focusout", (e) => {
      const to = e.relatedTarget as Node | null;
      if (!to || !el.contains(to)) {
        el.classList.remove("ah-slider-focused");
        if (!this.#dragging) { this.showTip(false); }
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
  /** setValue(v | [lo, hi] | "lo,hi"[, silent]) */
  setValue(v: number | string | readonly (number | string)[], silent?: boolean): void {
    const c = this.conf();
    const raw: readonly (number | string)[] = Array.isArray(v) ? v : String(v).split(",");
    let vals = raw.map((x) => this.snap(parseFloat(String(x))));
    if (c.range) { vals = [Math.min(vals[0], vals[1]), Math.max(vals[0], vals[1])]; }
    const old = this.element.getAttribute("data-ah-value");
    this.render(vals);
    if (!silent && this.element.getAttribute("data-ah-value") !== old) { this.fire("change"); }
  }

  private conf(): Conf {
    if (!this.#conf) {
      const el = this.element;
      const step = parseFloat(el.getAttribute("data-ah-step") || "") || 1;
      const max = parseFloat(el.getAttribute("data-ah-max") || "");
      this.#conf = {
        min: parseFloat(el.getAttribute("data-ah-min") || "") || 0,
        max: isNaN(max) ? 100 : max,
        step: step,
        decimals: (String(step).split(".")[1] || "").length,
        minRange: parseFloat(el.getAttribute("data-ah-min-range") || "") || 0,
        vertical: el.classList.contains("ah-slider-vertical"),
        range: el.classList.contains("ah-slider-range-slider")
      };
    }
    return this.#conf;
  }

  private values(): number[] {
    return String(this.element.getAttribute("data-ah-value") || "").split(",").map(parseFloat);
  }

  private snap(v: number): number {
    const c = this.conf();
    const n = Math.round((v - c.min) / c.step);
    const snapped = Math.max(c.min, Math.min(c.max, c.min + n * c.step));
    return parseFloat(snapped.toFixed(c.decimals));
  }

  private render(vals: readonly number[]): void {
    const el = this.element, c = this.conf();
    const ratio = (v: number): number => (v - c.min) / (c.max - c.min);
    const range = el.querySelector<HTMLElement>(".ah-slider-range");
    const end = el.querySelector<HTMLElement>(".ah-slider-thumb-end");
    const start = el.querySelector<HTMLElement>(".ah-slider-thumb-start");
    const pos = (t: HTMLElement | null, v: number): void => {
      if (!t) { return; }
      if (c.vertical) { t.style.top = frac(1 - ratio(v)); } else { t.style.left = frac(ratio(v)); }
    };
    const aria = (node: Element | null, v: number): void => {
      if (!node) { return; }
      node.setAttribute("aria-valuenow", String(v));
      node.setAttribute("aria-valuetext", String(v));
    };
    if (c.range) {
      pos(start, vals[0]);
      pos(end, vals[1]);
      if (c.vertical) {
        css(range, { bottom: fracCenter(ratio(vals[0])), height: frac(ratio(vals[1]) - ratio(vals[0])) });
      } else {
        css(range, { left: fracCenter(ratio(vals[0])), width: frac(ratio(vals[1]) - ratio(vals[0])) });
      }
      aria(start, vals[0]);
      aria(end, vals[1]);
    } else {
      pos(end, vals[0]);
      if (c.vertical) { css(range, { bottom: "0px", height: fracCenter(ratio(vals[0])) }); }
      else { css(range, { left: "0px", width: fracCenter(ratio(vals[0])) }); }
      aria(el, vals[0]);
    }
    // Value-bearing contract: data-ah-value + hidden input
    const v = vals.join(",");
    el.setAttribute("data-ah-value", v);
    el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = v; });
    this.tooltip();
  }

  private tooltip(): void {
    const el = this.element;
    const tip = el.querySelector<HTMLElement>(":scope > .ah-slider-tooltip");
    if (!tip) { return; }
    const first = this.#thumb === "start";
    const thumb = el.querySelector(first ? ".ah-slider-thumb-start" : ".ah-slider-thumb-end");
    const vals = this.values();
    tip.textContent = String(first ? vals[0] : vals[vals.length - 1]);
    if (!thumb) { return; }
    const root = el.getBoundingClientRect();
    const r = thumb.getBoundingClientRect();
    if (this.conf().vertical) {
      tip.style.top = (r.top + r.height / 2 - root.top) + "px";
    } else {
      tip.style.left = (r.left + r.width / 2 - root.left) + "px";
    }
  }

  // Set one thumb ("start" | "end") to v, keeping the range ordered.
  private set(which: Thumb | undefined, value: number): boolean {
    const c = this.conf();
    let vals = this.values();
    const v = this.snap(value);
    if (c.range) {
      if (which === "start") { vals[0] = Math.min(v, vals[1] - c.minRange); }
      else { vals[1] = Math.max(v, vals[0] + c.minRange); }
      vals[0] = Math.max(c.min, vals[0]);
      vals[1] = Math.min(c.max, vals[1]);
    } else {
      vals = [v];
    }
    const old = this.element.getAttribute("data-ah-value");
    this.render(vals);
    return this.element.getAttribute("data-ah-value") !== old;
  }

  private fromPointer(e: PointerEvent): number {
    const c = this.conf();
    // the track is always rendered (aihtml_slider)
    const r = (this.element.querySelector(".ah-slider-track") as Element).getBoundingClientRect();
    const ratio = c.vertical
      ? 1 - (e.clientY - r.top - THUMB / 2) / (r.height - THUMB)
      : (e.clientX - r.left - THUMB / 2) / (r.width - THUMB);
    return c.min + Math.max(0, Math.min(1, ratio)) * (c.max - c.min);
  }

  private disabled(): boolean {
    const el = this.element;
    return el.classList.contains("ah-slider-disabled") || el.getAttribute("aria-disabled") === "true";
  }

  private showTip(on: boolean): void {
    const tip = this.element.querySelector(":scope > .ah-slider-tooltip");
    if (tip) { tip.classList.toggle("ah-slider-tooltip-visible", on); }
  }

  private stepBy(which: Thumb, delta: number): void {
    const c = this.conf();
    const vals = this.values();
    const cur = c.range ? (which === "start" ? vals[0] : vals[1]) : vals[0];
    if (this.set(which, cur + delta)) {
      this.fire("input");
      this.fire("change");
    }
  }
}

AH.register("slider", SliderController);
