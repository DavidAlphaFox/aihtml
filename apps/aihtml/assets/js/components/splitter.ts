/* Behaviour of the splitter component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.ts.
   Value contract: data-ah-value "<pane0 %>,<pane1 %>", the hidden input,
   "input" while dragging and "change" when a change ends. Other events
   on the root (no detail): ah:resize-start, ah:resize, ah:collapsed,
   ah:expanded. */
import AH from "../core.ts";
import { setValue } from "./_lib_nav.ts";

// ------------------------------------------------------------------
// splitter (sigil splitter.cljs)
// ------------------------------------------------------------------
//
// The first pane's flex-basis is a fraction of the space left by the
// bar, so the split keeps its proportion when the container resizes.

/** The two panes and the bar between them (the server renders them). */
interface Parts { p0: HTMLElement; p1: HTMLElement; bar: HTMLElement; }

/** A bar drag in progress: where it started, pane 0's size then, and the
 *  value before it. */
interface Drag { start: number; size: number; value: string | null; }

/** "<a>,<b>": pane 0 and pane 1 in percent, one decimal. */
function format(f: number): string {
  const a = Math.round(f * 1000) / 10;
  const b = Math.round((100 - a) * 10) / 10;
  return a + "," + b;
}

class SplitterController extends AH.Controller {
  #parts: Parts | null = null;
  #horiz = false;
  #min0 = 0;
  #min1 = 0;
  /** Pane 0's share of the space left by the bar (0..1), null until set. */
  #frac: number | null = null;
  /** The share before a collapse, restored by expand. */
  #saved: number | null = null;
  #drag: Drag | null = null;

  override setup(): void {
    const el = this.element;
    const mins = (el.getAttribute("data-ah-min") || "0,0").split(",").map((x) => parseFloat(x) || 0);
    const panels = Array.from(el.querySelectorAll<HTMLElement>(":scope > .ah-splitter-panel"));
    const bar = el.querySelector<HTMLElement>(":scope > .ah-splitter-splitbar");
    this.#horiz = el.classList.contains("ah-splitter-horizontal");
    this.#min0 = mins[0];
    this.#min1 = mins[1] || 0;
    this.#frac = null;
    this.#saved = null;
    this.#drag = null;
    this.#parts = bar && panels.length >= 2 ? { p0: panels[0], p1: panels[1], bar: bar } : null;
    const parts = this.#parts;
    if (!parts) { return; }
    const avail = this.#avail(parts);
    // measure the initial split (pixels or percent) as a fraction
    if (avail > 0) {
      const v = el.getAttribute("data-ah-value");
      this.#apply(parts, v ? parseFloat(v) / 100 : this.#dim(parts.p0) / avail);
    }
    this.listen(parts.bar, "pointerdown", (e) => {
      const t = e.target;
      if (e.button !== 0 || !this.#enabled() ||
          (t instanceof Element && t.closest(".ah-splitter-collapse-btn"))) {
        return;
      }
      e.preventDefault();
      if (parts.bar.setPointerCapture) {
        try { parts.bar.setPointerCapture(e.pointerId); } catch { /* synthetic event */ }
      }
      this.#drag = { start: this.#horiz ? e.clientY : e.clientX, size: this.#dim(parts.p0),
                     value: el.getAttribute("data-ah-value") };
      if (el.classList.contains("ah-splitter-collapsed")) { this.#setCollapsed(parts, false); }
      el.classList.add("ah-splitter-dragging");
      this.fire("ah:resize-start");
    });
    this.listen(parts.bar, "pointermove", (e) => {
      const drag = this.#drag;
      if (!drag) { return; }
      const want = drag.size + (this.#horiz ? e.clientY : e.clientX) - drag.start;
      const max = this.#avail(parts) - this.#min1;
      parts.bar.classList.toggle("ah-splitbar-invalid", want <= this.#min0 || want >= max);
      this.#resize(parts, want);
      setValue(el, this.#format(), "input");
    });
    const end = (): void => {
      const drag = this.#drag;
      if (!drag) { return; }
      const before = drag.value;
      this.#drag = null;
      parts.bar.classList.remove("ah-splitbar-invalid");
      el.classList.remove("ah-splitter-dragging");
      const v = this.#format();
      if (v !== before) { setValue(el, v, "change"); }
      this.fire("ah:resize");
    };
    this.listen(parts.bar, "pointerup", end);
    this.listen(parts.bar, "pointercancel", end);
    this.delegate("click", ".ah-splitter-collapse-btn", (e) => {
      e.stopPropagation();
      if (this.#enabled() || el.classList.contains("ah-splitter-collapsed")) { this.#toggle(parts); }
    }, parts.bar);
    this.listen(parts.bar, "keydown", (e) => {
      if (e.target !== parts.bar || !this.#enabled()) { return; }
      const step = (parseInt(el.getAttribute("data-ah-step") || "", 10) || 10) * (e.shiftKey ? 5 : 1);
      const cur = this.#dim(parts.p0);
      const dec = this.#horiz ? "ArrowUp" : "ArrowLeft";
      const inc = this.#horiz ? "ArrowDown" : "ArrowRight";
      let px: number;
      switch (e.key) {
        case dec: px = cur - step; break;
        case inc: px = cur + step; break;
        case "Home": px = 0; break;
        case "End": px = Infinity; break;
        case "Enter": e.preventDefault(); this.#toggle(parts); return;
        default: return;
      }
      e.preventDefault();
      if (el.classList.contains("ah-splitter-collapsed")) { this.#setCollapsed(parts, false); }
      this.#resize(parts, px);
      const v = this.#format();
      if (v !== el.getAttribute("data-ah-value")) { setValue(el, v, "change"); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  // sizes: pane 0 in percent (a number or "30"); fires no change
  setSizes(pct: number | string): void {
    const parts = this.#parts;
    if (!parts) { return; }
    this.#setCollapsed(parts, false);
    this.#apply(parts, parseFloat(String(pct)) / 100);
    setValue(this.element, this.#format());
  }
  getSizes(): number[] {
    const parts = this.#parts;
    return parts ? [this.#dim(parts.p0), this.#dim(parts.p1)] : [];
  }
  collapse(): void {
    if (this.#parts && !this.element.classList.contains("ah-splitter-collapsed")) { this.#toggle(this.#parts); }
  }
  expand(): void {
    if (this.#parts && this.element.classList.contains("ah-splitter-collapsed")) { this.#toggle(this.#parts); }
  }

  #dim(node: HTMLElement): number { return this.#horiz ? node.offsetHeight : node.offsetWidth; }
  #avail(parts: Parts): number { return this.#dim(this.element) - this.#dim(parts.bar); }
  #format(): string { return format(this.#frac || 0); }

  #apply(parts: Parts, frac: number): void {
    const f = this.#frac = Math.max(0, Math.min(1, frac));
    const bar = this.#dim(parts.bar);
    parts.p0.style.flex = "0 0 calc((100% - " + bar + "px) * " + f.toFixed(5) + ")";
    parts.bar.setAttribute("aria-valuenow", String(Math.round(f * 100)));
  }

  // Resize pane 0 to px (clamped to the minimum sizes).
  #resize(parts: Parts, px: number): void {
    const avail = this.#avail(parts);
    if (avail <= 0) { return; }
    const clamped = Math.max(this.#min0, Math.min(avail - this.#min1, px));
    this.#apply(parts, clamped / avail);
  }

  #setCollapsed(parts: Parts, on: boolean): void {
    this.element.classList.toggle("ah-splitter-collapsed", on);
    const min = on ? "0px" : this.#min0 + "px";
    if (this.#horiz) { parts.p0.style.minHeight = min; } else { parts.p0.style.minWidth = min; }
  }

  #toggle(parts: Parts): void {
    const el = this.element;
    if (el.classList.contains("ah-splitter-collapsed")) {
      this.#setCollapsed(parts, false);
      this.#apply(parts, this.#saved !== null ? this.#saved : 0.5);
      this.fire("ah:expanded");
    } else {
      this.#saved = this.#frac;
      this.#setCollapsed(parts, true);
      this.#apply(parts, 0);
      this.fire("ah:collapsed");
    }
    setValue(el, this.#format(), "change");
  }

  #enabled(): boolean {
    const el = this.element;
    return !el.classList.contains("ah-splitter-disabled") && el.getAttribute("data-ah-resizable") !== "false";
  }
}

AH.register("splitter", SplitterController);
