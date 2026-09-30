/* Behaviour of range_selector (designs/04-components.md), after sigil's
 * form/range_selector.
 *
 *   range-selector   drag a marker or the bar between them; the markers
 *                    are ARIA sliders; input while dragging, change after
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change" (native events, detail: the value "lo,hi",
 * RangeSelectorValue).
 *
 * The server renders the whole first state, so setup only binds events.
 */
import AH from "../core.ts";
import { time12 } from "./_lib_date.ts";

/** Detail of input / change: the value "lo,hi". */
export type RangeSelectorValue = string;

/** data-ah-format: how the marker labels show a value
 *  (aihtml_range_selector:format/2): f the kind, n the decimals of
 *  "fixed", p / s a prefix and a suffix. */
interface Format {
  f?: string;
  n?: number;
  p?: string;
  s?: string;
  /** the currency symbol given; else the page language's */
  c?: string;
}

/** The settings and parts read in setup, and the current range. */
interface State {
  min: number;
  max: number;
  step: number;
  page: number;
  minSpan: number;
  format: Format;
  lo: number;
  hi: number;
  committed: string | null;
  track: HTMLElement | null;
  slider: HTMLElement | null;
  shutL: HTMLElement | null;
  shutR: HTMLElement | null;
  mL: HTMLElement | null;
  mR: HTMLElement | null;
  drag: AbortController | null;
}

type PointEvent = MouseEvent | TouchEvent;

function parseFormat(text: string | null): Format {
  let raw: unknown;
  try { raw = JSON.parse(text || "{}"); } catch { raw = {}; }
  if (!raw || typeof raw !== "object") { return {}; }
  const o = raw as Record<string, unknown>;
  const f: Format = {};
  if (typeof o.f === "string") { f.f = o.f; }
  if (typeof o.n === "number") { f.n = o.n; }
  if (typeof o.p === "string") { f.p = o.p; }
  if (typeof o.s === "string") { f.s = o.s; }
  if (typeof o.c === "string") { f.c = o.c; }
  return f;
}

function writeValue(el: Element, v: string): void {
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll<HTMLInputElement>(":scope > input[type=hidden]").forEach((h) => { h.value = v; });
}

function child(el: Element | null, cls: string): HTMLElement | null {
  if (!el) { return null; }
  return (Array.from(el.children).find((c) => c.classList.contains(cls)) as HTMLElement | undefined) || null;
}

// ------------------------------------------------------------------
// range-selector
// ------------------------------------------------------------------

const MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

function erlRound(v: number): number { return v < 0 ? -Math.round(-v) : Math.round(v); }

// The page language's short date pattern (yyyy, MM, M, dd, d; short_date/4
// in Erlang).
function shortDate(d: Date): string {
  const two = (n: number): string => (n < 10 ? "0" : "") + n;
  const m = d.getUTCMonth() + 1, day = d.getUTCDate();
  return AH.format("date_short", "M/d/yyyy").replace(/yyyy|MM|M|dd|d/g, (t) =>
    t === "yyyy" ? String(d.getUTCFullYear()) : t === "MM" ? two(m) : t === "M" ? String(m)
      : t === "dd" ? two(day) : String(day));
}

// The same formats as aihtml_range_selector:format/2.
function rsFormat(v: number, f: Format): string {
  let s: string;
  switch (f.f) {
    case "fixed": s = v.toFixed(f.n); break;
    case "currency": {
      const d = erlRound(v);
      s = (f.c ?? AH.format("currency", "$")) + (d < 0 ? "-" : "") +
        String(Math.abs(d)).replace(/\B(?=(\d{3})+(?!\d))/g, AH.format("group", ","));
      break;
    }
    case "date": {
      s = shortDate(new Date(Math.floor(v)));
      break;
    }
    case "month": s = AH.format("months_short", MONTHS)[new Date(Math.floor(v)).getUTCMonth()]; break;
    case "time": {
      const d = new Date(Math.floor(v));
      const h = d.getUTCHours();
      s = time12(((h % 12) || 12) + ":" + (d.getUTCMinutes() < 10 ? "0" : "") + d.getUTCMinutes(),
                 h >= 12 ? AH.format("pm", "PM") : AH.format("am", "AM"));
      break;
    }
    default:
      s = Math.abs(v - erlRound(v)) < 0.001 ? String(erlRound(v)) : v.toFixed(2);
  }
  return (f.p || "") + s + (f.s || "");
}

function tidy(v: number): number { return parseFloat(v.toFixed(9)); }

function rsSnap(st: State, v: number): number {
  const x = st.min + Math.round((v - st.min) / st.step) * st.step;
  return tidy(Math.max(st.min, Math.min(st.max, x)));
}

function pct(st: State, v: number): number { return tidy((v - st.min) / (st.max - st.min) * 100); }

function pointX(e: PointEvent): number {
  if ("touches" in e) {
    const t = e.touches[0] || e.changedTouches[0];
    return t ? t.clientX : NaN;
  }
  return e.clientX;
}

function css(node: HTMLElement | null, props: Partial<Record<"left" | "width", string>>): void {
  if (node) { Object.assign(node.style, props); }
}

class RangeSelectorController extends AH.Controller {
  #st!: State;

  override setup(): void {
    const el = this.element;
    const num = (a: string): number => parseFloat(el.getAttribute(a) || "");
    const v = (el.getAttribute("data-ah-value") || "").split(",");
    const track = child(el, "ah-range-selector-track");
    const st: State = this.#st = {
      min: num("data-ah-min"), max: num("data-ah-max"), step: num("data-ah-step") || 1,
      page: num("data-ah-page") || 10, minSpan: num("data-ah-min-span") || 0,
      format: parseFormat(el.getAttribute("data-ah-format")),
      lo: parseFloat(v[0]), hi: parseFloat(v[1]),
      committed: el.getAttribute("data-ah-value"),
      track: track,
      slider: child(track, "ah-range-selector-slider"),
      shutL: child(track, "ah-range-selector-shutter-left"),
      shutR: child(track, "ah-range-selector-shutter-right"),
      mL: child(track, "ah-range-selector-marker-left"),
      mR: child(track, "ah-range-selector-marker-right"),
      drag: null
    };
    if (!track) { return; }

    const onMarker = (e: PointEvent, marker: HTMLElement): void => {
      if (this.disabled()) { return; }
      const left = marker.classList.contains("ah-range-selector-marker-left");
      marker.focus();
      this.drag(e, (x) => { this.moveEnd(left, this.valueAt(x), false); });
    };
    this.delegate("mousedown", ".ah-range-selector-marker", onMarker, track);
    this.delegate("touchstart", ".ah-range-selector-marker", onMarker, track);
    if (st.slider) {
      const onBar = (e: PointEvent): void => {
        if (this.disabled()) { return; }
        const span = tidy(st.hi - st.lo), grab = this.valueAt(pointX(e)) - st.lo;
        this.drag(e, (x) => {
          const lo = rsSnap(st, Math.max(st.min, Math.min(st.max - span, this.valueAt(x) - grab)));
          this.set(lo, Math.min(st.max, tidy(lo + span)), false);
        });
      };
      this.listen(st.slider, "mousedown", onBar);
      this.listen(st.slider, "touchstart", onBar);
    }
    this.delegate("keydown", ".ah-range-selector-marker", (e, marker) => {
      if (this.disabled()) { return; }
      const left = marker.classList.contains("ah-range-selector-marker-left"), cur = left ? st.lo : st.hi;
      let to: number;
      switch (e.key) {
        case "ArrowRight": case "ArrowUp": to = cur + st.step; break;
        case "ArrowLeft": case "ArrowDown": to = cur - st.step; break;
        case "PageUp": to = cur + st.page; break;
        case "PageDown": to = cur - st.page; break;
        case "Home": to = st.min; break;
        case "End": to = st.max; break;
        default: return;
      }
      e.preventDefault();
      this.moveEnd(left, to, true);
    }, track);
  }

  override teardown(): void { this.endDrag(); }

  // methods (aihtml_action:call/4, AH.invoke)
  /** "lo,hi" or [lo, hi]; no event. */
  setValue(v: string | readonly (number | string)[]): void {
    const st = this.#st;
    const parts: readonly (number | string)[] = typeof v === "string" ? v.split(",") : v;
    let lo = rsSnap(st, parseFloat(String(parts[0]))), hi = rsSnap(st, parseFloat(String(parts[1])));
    if (lo > hi) { const t = lo; lo = hi; hi = t; }
    st.lo = lo; st.hi = hi;
    this.layout();
    st.committed = this.element.getAttribute("data-ah-value");
  }
  getValue(): [number, number] { return [this.#st.lo, this.#st.hi]; }

  private disabled(): boolean { return this.element.classList.contains("ah-range-selector-disabled"); }

  private layout(): void {
    const st = this.#st, a = pct(st, st.lo), b = pct(st, st.hi);
    css(st.slider, { left: a + "%", width: tidy(b - a) + "%" });
    css(st.shutL, { width: a + "%" });
    css(st.shutR, { left: b + "%", width: tidy(100 - b) + "%" });
    const markers: [HTMLElement | null, number, number][] = [[st.mL, st.lo, a], [st.mR, st.hi, b]];
    markers.forEach(([marker, value, at]) => {
      if (!marker) { return; }
      const t = rsFormat(value, st.format);
      marker.style.left = at + "%";
      marker.setAttribute("aria-valuenow", String(value));
      marker.setAttribute("aria-valuetext", t);
      const label = child(marker, "ah-range-selector-marker-value");
      if (label) { label.textContent = t; }
    });
    writeValue(this.element, st.lo + "," + st.hi);
  }

  // Set lo / hi (already bounded); input when it changed, change if asked.
  private set(lo: number, hi: number, change: boolean): void {
    const st = this.#st, el = this.element, old = el.getAttribute("data-ah-value");
    st.lo = lo; st.hi = hi;
    this.layout();
    const v = el.getAttribute("data-ah-value") || "";
    if (v !== old) { this.fire<RangeSelectorValue>("input", v); }
    if (change && v !== st.committed) {
      st.committed = v;
      this.fire<RangeSelectorValue>("change", v);
    }
  }

  // Move one end to v, kept min_span away from the other.
  private moveEnd(left: boolean, value: number, change: boolean): void {
    const st = this.#st;
    const v = rsSnap(st, value);
    if (left) {
      this.set(Math.max(st.min, Math.min(v, tidy(st.hi - st.minSpan))), st.hi, change);
    } else {
      this.set(st.lo, Math.min(st.max, Math.max(v, tidy(st.lo + st.minSpan))), change);
    }
  }

  private valueAt(x: number): number {
    const st = this.#st;
    if (!st.track) { return st.min; }
    const r = st.track.getBoundingClientRect();
    const p = r.width > 0 ? Math.max(0, Math.min(1, (x - r.left) / r.width)) : 0;
    return st.min + p * (st.max - st.min);
  }

  private endDrag(): void {
    const st = this.#st;
    if (st && st.drag) { st.drag.abort(); st.drag = null; }
  }

  // Follow the pointer on the document until release.
  private drag(e: PointEvent, move: (x: number) => void): void {
    const st = this.#st, el = this.element;
    if (e.type === "mousedown") {
      if ((e as MouseEvent).button !== 0) { return; }
      e.preventDefault();
    }
    this.endDrag();
    const ac = st.drag = new AbortController(), o = { signal: ac.signal };
    const onMove = (me: PointEvent): void => {
      if (me.type === "mousemove") { me.preventDefault(); }
      move(pointX(me));
    };
    const onUp = (): void => {
      this.endDrag();
      el.classList.remove("ah-range-selector-dragging");
      this.set(st.lo, st.hi, true);
    };
    document.addEventListener("mousemove", onMove, o);
    document.addEventListener("touchmove", onMove, o);
    ["mouseup", "touchend", "touchcancel"].forEach((t) => { document.addEventListener(t, onUp, o); });
    el.classList.add("ah-range-selector-dragging");
  }
}

AH.register("range-selector", RangeSelectorController);
