/* Behaviour of the scrollbar component (designs/04-components.md),
 * ported from sigil's scrollbar (cljs). A standalone bar keeps
 * its value in data-ah-value on the root, mirrors it into a hidden input
 * when there is one, and fires "change" on the root when the user changes
 * it; methods called by the server update the value without firing
 * "change". Drags use pointer capture, so nothing is bound on document.
 */
import AH from "../core.ts";
import type { Controller } from "../core.ts";
import { capture, num, setValue } from "./_lib_scroll.ts";

// ------------------------------------------------------------------
// Scrollbar (sigil layout/scrollbar, and the bars of sigil's panel)
// ------------------------------------------------------------------
//
// One bar is .ah-scrollbar with five parts: up button, track before the
// thumb, thumb, track after it, down button. layout() sizes them from a
// value in [min, max] exactly as sigil's arrange! does.

const SB = "ah-scrollbar";
const SB_BTN = 14;

/** What a bar shows: a value in [min, max] and its steps. */
interface BarOpts {
  min: number; max: number; value: number;
  step: number; large: number; thumbMin: number; buttons: boolean;
}

/** Geometry of a bar for a value: track length, thumb size and position. */
interface Geom { vert: boolean; track: number; ts: number; tp: number; }

/** "input" while dragging, "change" otherwise and "end" when a drag ends. */
type Phase = "input" | "change" | "end";

/** What a bar drives: opts() reads its state, set() applies a value
 *  (before: the value when a drag started, with "end"), start() runs when
 *  a drag starts. */
interface BarApi {
  opts(): BarOpts;
  set(v: number, phase: Phase, before?: number): void;
  start?(): void;
}

interface Parts { up: HTMLElement; tu: HTMLElement; thumb: HTMLElement; td: HTMLElement; down: HTMLElement; }

function parts(bar: Element): Parts {
  // the server renders all five parts of a bar
  const part = (name: string): HTMLElement => bar.querySelector(":scope > ." + SB + "-" + name) as HTMLElement;
  return { up: part("btn-up"), tu: part("track-up"), thumb: part("thumb"), td: part("track-down"),
           down: part("btn-down") };
}

function vertical(bar: Element): boolean { return bar.classList.contains(SB + "-vertical"); }

function geom(bar: Element, o: BarOpts): Geom {
  const vert = vertical(bar);
  const total = vert ? bar.clientHeight : bar.clientWidth;
  const track = Math.max(0, total - (o.buttons ? 2 * SB_BTN : 0));
  const range = o.max - o.min;
  let ts = range <= 0 ? track : Math.max(o.thumbMin, track * track / (track + range));
  ts = Math.min(ts, track);
  const tp = range <= 0 ? 0 : (o.value - o.min) / range * (track - ts);
  return { vert, track, ts, tp };
}

function layout(bar: Element, o: BarOpts): Geom {
  const g = geom(bar, o), p = parts(bar), dim = g.vert ? "height" : "width";
  p.up.style.display = p.down.style.display = o.buttons ? "" : "none";
  p.tu.style[dim] = g.tp + "px";
  p.thumb.style[dim] = g.ts + "px";
  p.td.style[dim] = Math.max(0, g.track - g.ts - g.tp) + "px";
  return g;
}

function posToValue(bar: Element, o: BarOpts, pos: number): number {
  const g = geom(bar, o), free = g.track - g.ts;
  return free <= 0 ? o.min : o.min + Math.max(0, Math.min(free, pos)) / free * (o.max - o.min);
}

// Repeat fn while a button is held: once now, again after 300ms, then
// every 50ms (sigil util/start-repeat-timer!).
function repeat(node: Element, e: PointerEvent, fn: () => void): void {
  fn();
  let iv: ReturnType<typeof setInterval> | undefined;
  const t = setTimeout(() => { iv = setInterval(fn, 50); }, 300);
  capture(node, e);
  const off = new AbortController();
  const end = (): void => {
    clearTimeout(t);
    clearInterval(iv);
    off.abort();
  };
  ["pointerup", "pointercancel", "lostpointercapture"].forEach((type) => {
    node.addEventListener(type, end, { signal: off.signal });
  });
}

function primary(e: PointerEvent): boolean {
  return e.button === undefined || e.button === 0;
}

/** One wired bar: its buttons, tracks and the thumb drag. The listeners
 *  go through ctrl.listen, so they are removed on teardown. */
class BarBinding {
  #drag: { start: number; pos: number; vert: boolean; value: number } | null = null;

  constructor(ctrl: Controller, private readonly bar: Element, private readonly api: BarApi) {
    const p = parts(bar);
    [p.up, p.down].forEach((btn) => {
      ctrl.listen(btn, "pointerdown", (e) => {
        if (!primary(e)) { return; }
        e.preventDefault();
        const dir = btn === p.up ? -1 : 1;
        repeat(btn, e, () => { const o = api.opts(); api.set(o.value + dir * o.step, "change"); });
      });
    });
    [p.tu, p.td].forEach((track) => {
      ctrl.listen(track, "pointerdown", (e) => {
        if (!primary(e)) { return; }
        e.preventDefault();
        const o = api.opts();
        api.set(o.value + (track === p.tu ? -1 : 1) * o.large, "change");
      });
    });
    ctrl.listen(p.thumb, "pointerdown", (e) => {
      if (!primary(e)) { return; }
      e.preventDefault();
      const o = api.opts(), g = geom(bar, o);
      this.#drag = { start: g.vert ? e.clientY : e.clientX, pos: g.tp, vert: g.vert, value: o.value };
      capture(p.thumb, e);
      p.thumb.classList.add(SB + "-thumb-pressed");
      if (api.start) { api.start(); }
    });
    ctrl.listen(p.thumb, "pointermove", (e) => {
      const d = this.#drag;
      if (!d) { return; }
      const cur = d.vert ? e.clientY : e.clientX;
      api.set(posToValue(this.bar, this.api.opts(), d.pos + cur - d.start), "input");
    });
    const end = (): void => {
      if (!this.#drag) { return; }
      const before = this.#drag.value;
      this.#drag = null;
      p.thumb.classList.remove(SB + "-thumb-pressed");
      api.set(api.opts().value, "end", before);
    };
    ctrl.listen(p.thumb, "pointerup", end);
    ctrl.listen(p.thumb, "pointercancel", end);
  }
}

// --- scroll area --------------------------------------------------------

interface AreaParts { vp: HTMLElement; v: HTMLElement; h: HTMLElement; }

function areaParts(el: Element): AreaParts {
  // a scroll area (data-area) always has its viewport and both bars
  const q = (s: string): HTMLElement => el.querySelector(":scope > ." + SB + "-" + s) as HTMLElement;
  return { vp: q("viewport"), v: q("vertical"), h: q("horizontal") };
}

function areaOpts(el: Element, vp: HTMLElement, vert: boolean): BarOpts {
  const max = vert ? vp.scrollHeight - vp.clientHeight : vp.scrollWidth - vp.clientWidth;
  const page = vert ? vp.clientHeight : vp.clientWidth;
  return {
    min: 0, max: Math.max(0, max),
    value: vert ? vp.scrollTop : vp.scrollLeft,
    step: num(el, "data-step", 10) * 3, large: Math.max(10, page * 0.9),
    thumbMin: num(el, "data-thumb-min", 10),
    buttons: el.getAttribute("data-buttons") !== "false"
  };
}

// Which bars the content needs: showing one narrows the viewport, which
// may make the other one necessary, so settle it in two passes.
function areaLayout(el: Element): void {
  const p = areaParts(el), vp = p.vp;
  let needV = false, needH = false;
  for (let i = 0; i < 2; i++) {
    el.classList.toggle(SB + "-area-v", needV);
    el.classList.toggle(SB + "-area-h", needH);
    needV = vp.scrollHeight > vp.clientHeight + 1;
    needH = vp.scrollWidth > vp.clientWidth + 1;
  }
  el.classList.toggle(SB + "-area-v", needV);
  el.classList.toggle(SB + "-area-h", needH);
  if (needV) { layout(p.v, areaOpts(el, vp, true)); }
  if (needH) { layout(p.h, areaOpts(el, vp, false)); }
}

function areaSync(el: Element): void {
  const p = areaParts(el);
  if (el.classList.contains(SB + "-area-v")) { layout(p.v, areaOpts(el, p.vp, true)); }
  if (el.classList.contains(SB + "-area-h")) { layout(p.h, areaOpts(el, p.vp, false)); }
}

class ScrollbarController extends AH.Controller {
  #ro: ResizeObserver | null = null;

  override setup(): void {
    this.#ro = null;
    if (this.element.hasAttribute("data-area")) { this.area(); } else { this.standalone(); }
  }

  override teardown(): void {
    if (this.#ro) { this.#ro.disconnect(); this.#ro = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(v: number | string): void {
    if (!this.element.hasAttribute("data-area")) { this.set(v, null); }
  }
  getValue(): number { return num(this.element, "data-ah-value", 0); }
  setMax(max: number | string): void {
    const el = this.element;
    el.setAttribute("data-max", String(max));
    el.setAttribute("aria-valuemax", String(max));
    this.set(num(el, "data-ah-value", 0), null);
    layout(this.bar(), this.opts());
  }
  scrollTo(x?: number, y?: number): void {
    const vp = this.element.querySelector<HTMLElement>(":scope > ." + SB + "-viewport");
    if (vp) {
      vp.scrollLeft = x || 0;
      vp.scrollTop = y || 0;
    }
  }
  refresh(): void {
    const el = this.element;
    if (el.hasAttribute("data-area")) { areaLayout(el); } else { layout(this.bar(), this.opts()); }
  }

  private observe(targets: (Element | null)[], fn: () => void): void {
    if (window.ResizeObserver) {
      const ro = new ResizeObserver(() => { fn(); });
      this.#ro = ro;
      targets.forEach((t) => { if (t) { ro.observe(t); } });
    } else {
      this.listen(window, "resize", fn);
    }
  }

  // --- standalone bar ---------------------------------------------------

  private opts(): BarOpts {
    const el = this.element;
    return {
      min: num(el, "data-min", 0), max: num(el, "data-max", 1000),
      value: num(el, "data-ah-value", 0),
      step: num(el, "data-step", 10), large: num(el, "data-large-step", 50),
      thumbMin: num(el, "data-thumb-min", 10),
      buttons: el.getAttribute("data-buttons") !== "false"
    };
  }

  // a standalone scrollbar always renders its bar
  private bar(): HTMLElement { return this.element.querySelector(":scope > ." + SB) as HTMLElement; }

  private set(raw: number | string, event: Phase | null): boolean {
    const el = this.element;
    const o = this.opts();
    let v = Math.max(o.min, Math.min(o.max, +raw || 0));
    const integral = o.min % 1 === 0 && o.max % 1 === 0 && o.step % 1 === 0 && o.large % 1 === 0;
    if (integral) { v = Math.round(v); }
    const changed = v !== o.value;
    if (changed) {
      setValue(el, String(v));
      el.setAttribute("aria-valuenow", String(v));
    }
    o.value = v;
    layout(this.bar(), o);
    if (changed && event) { this.fire(event); }
    return changed;
  }

  private standalone(): void {
    const el = this.element;
    const bar = this.bar();
    const disabled = (): boolean => el.classList.contains(SB + "-disabled");
    new BarBinding(this, bar, {
      opts: () => this.opts(),
      set: (v, phase, before) => {
        if (disabled()) { return; }
        if (phase === "end") {
          if (String(before) !== el.getAttribute("data-ah-value")) { this.fire("change"); }
          return;
        }
        this.set(v, phase);
      },
      start: () => { el.focus({ preventScroll: true }); }
    });
    this.listen(el, "keydown", (e) => {
      if (e.target !== el || disabled()) { return; }
      const o = this.opts(), vert = vertical(bar);
      let to: number;
      switch (e.key) {
        case "ArrowLeft": if (vert) { return; } to = o.value - o.step; break;
        case "ArrowRight": if (vert) { return; } to = o.value + o.step; break;
        case "ArrowUp": if (!vert) { return; } to = o.value - o.step; break;
        case "ArrowDown": if (!vert) { return; } to = o.value + o.step; break;
        case "PageUp": to = o.value - o.large; break;
        case "PageDown": to = o.value + o.large; break;
        case "Home": to = o.min; break;
        case "End": to = o.max; break;
        default: return;
      }
      e.preventDefault();
      this.set(to, "change");
    });
    const relayout = (): void => { layout(bar, this.opts()); };
    this.observe([el], relayout);
    relayout();
  }

  // --- scroll area --------------------------------------------------------

  private area(): void {
    const el = this.element;
    const p = areaParts(el);
    ([[p.v, true], [p.h, false]] as const).forEach(([bar, vert]) => {
      new BarBinding(this, bar, {
        opts: () => areaOpts(el, p.vp, vert),
        set: (v, phase) => {
          if (phase === "end" || el.classList.contains(SB + "-disabled")) { return; }
          if (vert) { p.vp.scrollTop = v; } else { p.vp.scrollLeft = v; }
          areaSync(el);
        }
      });
    });
    this.listen(p.vp, "scroll", () => { areaSync(el); });
    const content = p.vp.querySelector(":scope > ." + SB + "-content");
    this.observe([p.vp, content], () => { areaLayout(el); });
    areaLayout(el);
  }
}

AH.register("scrollbar", ScrollbarController);
