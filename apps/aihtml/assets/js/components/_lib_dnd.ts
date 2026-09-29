/* Internal: what the drag and drop behaviours share (sortable.ts,
 * dragdrop.ts), ported from sigil's layout/sortable and layout/dragdrop.
 * Pointer events (mouse, pen, touch) instead of sigil's mouse + touch
 * sequence; one drag at a time on the page (Drag.current, shared by both
 * components), tracked on the document and unbound when the drag ends or
 * the component is destroyed. Everything takes and returns elements;
 * fire(target, type, detail) dispatches the components' native events.
 */

const EDGE = 20, SPEED = 10;   // auto scroll: edge width and px per frame

/** px of movement before a press becomes a drag */
export const DISTANCE = 5;

/** A rectangle in page coordinates. */
export interface PageRect { left: number; top: number; right: number; bottom: number; width: number; height: number; }

export function pageRect(el: Element): PageRect {
  const r = el.getBoundingClientRect();
  const sx = window.pageXOffset, sy = window.pageYOffset;
  return { left: r.left + sx, top: r.top + sy, right: r.right + sx, bottom: r.bottom + sy,
           width: r.width, height: r.height };
}

// Page coordinates of a pointer event (from clientX, which every
// pointer event has, synthetic ones included).
export function px(e: MouseEvent): number { return e.clientX + window.pageXOffset; }
export function py(e: MouseEvent): number { return e.clientY + window.pageYOffset; }

export function inside(x: number, y: number, r: PageRect): boolean {
  return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
}

// The copy that follows the pointer lives in <body>, outside the scope of
// the custom properties it inherited (skins, a themed container), so they
// are copied onto it, as sigil does.
function copyVars(src: Element, dst: HTMLElement): void {
  const cs = window.getComputedStyle(src);
  for (let i = 0; i < cs.length; i++) {
    const p = cs.item(i);
    if (p && p.indexOf("--") === 0) { dst.style.setProperty(p, cs.getPropertyValue(p)); }
  }
}

/** A copy of el following the pointer, in <body>. */
export function floatingCopy(el: HTMLElement, cls: string, opacity: number): HTMLElement {
  const r = pageRect(el);
  const copy = el.cloneNode(true) as HTMLElement;
  copy.removeAttribute("id");
  copy.querySelectorAll("[id]").forEach((n) => { n.removeAttribute("id"); });
  copy.removeAttribute("tabindex");
  copy.setAttribute("aria-hidden", "true");
  copy.className += " " + cls;
  copyVars(el, copy);
  Object.assign(copy.style, { position: "absolute", margin: "0", boxSizing: "border-box",
                              width: r.width + "px", height: r.height + "px",
                              left: r.left + "px", top: r.top + "px", opacity: String(opacity),
                              zIndex: "999999", pointerEvents: "none" });
  document.body.appendChild(copy);
  return copy;
}

export function scrollParent(el: Element): HTMLElement | null {
  for (let cur = el.parentElement; cur && cur !== document.body; cur = cur.parentElement) {
    const s = window.getComputedStyle(cur);
    if ((/auto|scroll/.test(s.overflowY) && cur.scrollHeight > cur.clientHeight) ||
        (/auto|scroll/.test(s.overflowX) && cur.scrollWidth > cur.clientWidth)) {
      return cur;
    }
  }
  return null;
}

// Scroll the nearest scrollable ancestor (or the window) when the
// pointer is near its edge.
export function autoScroll(box: HTMLElement | null, cx: number, cy: number): void {
  if (box) {
    const r = box.getBoundingClientRect();
    if (cy - r.top < EDGE) { box.scrollTop -= SPEED; }
    else if (r.bottom - cy < EDGE) { box.scrollTop += SPEED; }
    if (cx - r.left < EDGE) { box.scrollLeft -= SPEED; }
    else if (r.right - cx < EDGE) { box.scrollLeft += SPEED; }
  }
  if (cy < EDGE) { window.scrollBy(0, -SPEED); }
  else if (window.innerHeight - cy < EDGE) { window.scrollBy(0, SPEED); }
}

// Swallow the click that follows a drag (items may be links).
export function swallowClick(): void {
  const stop = (e: Event): void => { e.stopPropagation(); e.preventDefault(); };
  document.addEventListener("click", stop, true);
  setTimeout(() => { document.removeEventListener("click", stop, true); }, 0);
}

export function editable(t: EventTarget | null): boolean {
  return t instanceof Element &&
    !!t.closest("input, textarea, select, [contenteditable=''], [contenteditable=true]");
}

// Say text in the live region `live' (an element; nothing without one).
export function announce(live: Element | null, text: string): void {
  if (!live) { return; }
  live.textContent = "";
  setTimeout(() => { live.textContent = text; }, 20);
}

export function label(el: Element): string {
  return el.getAttribute("aria-label") || (el.textContent || "").trim().replace(/\s+/g, " ").slice(0, 60);
}

// A native bubbling, cancelable event on target.
export function fire(target: EventTarget, type: string, detail?: unknown): boolean {
  return target.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

/**
 * The drag in progress (one per page, Drag.current): a pointer drag
 * tracked on the document (track) or a keyboard drag (claim). el is the
 * component root it belongs to; raf a pending animation frame, dropped
 * when the drag ends. Subclasses say what a cancel does.
 */
export abstract class Drag {
  static current: Drag | null = null;
  private static tracking: AbortController | null = null;   // the document listeners

  raf = 0;

  constructor(readonly el: HTMLElement) {}

  /** What a cancel (Escape, pointercancel, cancelFor) does after the
   *  drag ended. */
  protected abstract cancelled(): void;

  /** End the drag and cancel it. */
  cancel(): void {
    Drag.untrack();
    this.cancelled();
  }

  /** Make this the page's drag, without document listeners (keyboard). */
  claim(): void { Drag.current = this; }

  /** Make this the page's drag and follow pointer pointerId: move and
   *  end get its events; pointercancel and Escape cancel. */
  track(pointerId: number, move: (e: PointerEvent) => void, end: (e: PointerEvent) => void): void {
    Drag.current = this;
    if (Drag.tracking) { Drag.tracking.abort(); }
    Drag.tracking = new AbortController();
    const o = { signal: Drag.tracking.signal };
    document.addEventListener("pointermove", (e) => {
      if (e.pointerId === pointerId) { move(e); }
    }, o);
    document.addEventListener("pointerup", (e) => {
      if (e.pointerId === pointerId) { Drag.untrack(); end(e); }
    }, o);
    document.addEventListener("pointercancel", (e) => {
      if (e.pointerId === pointerId) { this.cancel(); }
    }, o);
    document.addEventListener("keydown", (e) => {
      if (e.key === "Escape") { e.preventDefault(); this.cancel(); }
    }, o);
  }

  /** No drag any more: document listeners off, pending frame dropped. */
  static untrack(): void {
    if (Drag.tracking) { Drag.tracking.abort(); Drag.tracking = null; }
    if (Drag.current && Drag.current.raf) { cancelAnimationFrame(Drag.current.raf); }
    Drag.current = null;
  }

  /** Cancel the drag in progress if it belongs to el. */
  static cancelFor(el: Element): void {
    if (Drag.current && Drag.current.el === el) { Drag.current.cancel(); }
  }
}
