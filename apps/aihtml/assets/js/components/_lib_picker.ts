/* Shared by the popup pickers (colorpicker.ts, timepicker.ts, ported from
 * sigil): the popup a field toggles (PickerPopup), pointer drags and small
 * helpers. `cls` is the component's root class ("ah-timepicker"); its
 * popup is the root's child `.<cls>-popup` and the root gets `<cls>-open`
 * while it shows. Functions taking `ctl` take the component's
 * AH.Controller: listeners bound through it go away on teardown. */
import AH from "../core.ts";
import type { Controller, FloatHandle } from "../core.ts";

/** A pointer position in viewport coordinates. */
export interface Point { x: number; y: number; }

let seq = 0;

/** A page-unique id: prefix, a sequence number and a random suffix. */
export function uid(prefix: string): string {
  seq += 1;
  return prefix + seq + "-" + Math.random().toString(36).slice(2, 7);
}

export function clamp(v: number, lo: number, hi: number): number {
  return Math.max(lo, Math.min(hi, v));
}

export function round2(n: number): number { return Math.round(n * 100) / 100; }

export function disabled(el: Element): boolean {
  return el.getAttribute("aria-disabled") === "true";
}

/** Keep the inner fields' native input/change events inside the
 *  component (the root's own events have the root as target). */
export function fenceNativeEvents(ctl: Controller<Element>): void {
  const el = ctl.element;
  const stop = (e: Event): void => {
    const t = e.target;
    if (t !== el && t instanceof Element && t.matches("input")) { e.stopPropagation(); }
  };
  ctl.listen(el, "input", stop);
  ctl.listen(el, "change", stop);
}

/** The popup child of a picker root, or null. */
export function popupOf(el: Element, cls: string): HTMLElement | null {
  for (let c = el.firstElementChild; c; c = c.nextElementSibling) {
    if (c instanceof HTMLElement && c.classList.contains(cls + "-popup")) { return c; }
  }
  return null;
}

/** The pointer (or first touch) position of an event. */
export function pointerXY(e: MouseEvent | TouchEvent): Point {
  const t: { clientX: number; clientY: number } | undefined = "touches" in e ? e.touches[0] : e;
  return t ? { x: t.clientX, y: t.clientY } : { x: NaN, y: NaN };
}

/** Pointer drag on one element: start(e) (returning false declines the
 *  drag), move(e), end(e). Uses pointer capture, so nothing is bound on
 *  document. */
export function drag(ctl: Controller<Element>, target: HTMLElement | SVGElement | null,
                     start: (e: PointerEvent) => boolean | void,
                     move: (e: PointerEvent) => void,
                     end: (e: PointerEvent) => void): void {
  if (!target) { return; }
  const el = ctl.element;
  // explicit event type: listen's element overload does not take SVG
  // elements (the timepicker's clock)
  ctl.listen<PointerEvent>(target, "pointerdown", (e) => {
    if (disabled(el) || (e.button !== undefined && e.button !== 0)) { return; }
    if (start(e) === false) { return; }
    e.preventDefault();
    if (e.pointerId !== undefined && target.setPointerCapture) {
      try { target.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
    }
    target.focus({ preventScroll: true });
    const ac = new AbortController();
    const up = (u: PointerEvent): void => { ac.abort(); end(u); };
    const t: HTMLElement = target as HTMLElement;   // the same event map for SVG elements
    t.addEventListener("pointermove", (m) => { move(m); }, { signal: ac.signal });
    t.addEventListener("pointerup", up, { signal: ac.signal });
    t.addEventListener("pointercancel", up, { signal: ac.signal });
  });
}

/** The popup a picker's field toggles: placed by AH.float (below, above
 *  when there is no room), closed by Escape (the component's) or a press
 *  outside. One per controller; stop() it on teardown. */
export class PickerPopup {
  readonly #ctl: Controller<Element>;
  readonly #cls: string;
  #float: FloatHandle | null = null;
  #outside: AbortController | null = null;

  constructor(ctl: Controller<Element>, cls: string) {
    this.#ctl = ctl;
    this.#cls = cls;
  }

  /** The popup element, or null when the component has none (inline). */
  get popup(): HTMLElement | null { return popupOf(this.#ctl.element, this.#cls); }

  get isOpen(): boolean {
    const p = this.popup;
    return !!p && !p.hidden;
  }

  /** Show the popup (onOpen runs first); opener gets aria-expanded. */
  open(opener: HTMLElement | null, onOpen?: () => void): void {
    const el = this.#ctl.element;
    const p = this.popup;
    if (!p || !p.hidden || disabled(el)) { return; }
    if (onOpen) { onOpen(); }
    p.hidden = false;
    el.classList.add(this.#cls + "-open");
    if (opener) { opener.setAttribute("aria-expanded", "true"); }
    this.stop();
    // Fixed positioning at the field (the input area or the trigger), so
    // an overflow:hidden ancestor such as a card does not clip it; flips
    // above when there is no room below and follows scrolling.
    this.#float = AH.float(p, el.firstElementChild, { placement: "bottom", align: "start", offset: 4 });
    const ac = new AbortController();
    this.#outside = ac;
    const outside = (e: Event): void => {
      if (!(e.target instanceof Node && el.contains(e.target))) { this.close(opener, false); }
    };
    ["mousedown", "touchstart", "focusin"].forEach((t) => {
      document.addEventListener(t, outside, { signal: ac.signal });
    });
  }

  /** Hide the popup; refocus puts the focus back on the opener. */
  close(opener: HTMLElement | null, refocus: boolean): void {
    const el = this.#ctl.element;
    const p = this.popup;
    this.stop();
    if (!p || p.hidden) { return; }
    p.hidden = true;
    el.classList.remove(this.#cls + "-open");
    if (opener) {
      opener.setAttribute("aria-expanded", "false");
      if (refocus) { opener.focus(); }
    }
  }

  /** Remove the outside-press listeners and stop the float (close,
   *  teardown). */
  stop(): void {
    if (this.#outside) { this.#outside.abort(); this.#outside = null; }
    if (this.#float) { this.#float.stop(); this.#float = null; }
  }
}
