/* Internal: what the overlay behaviours share (tooltip.ts, popover.ts,
 * drawer.ts, sheet.ts, window.ts, toast.ts, notification.ts), ported
 * from sigil's internal/common and scroll_lock:
 *   - the overlay layer (OverlayLayer, one instance: overlays): one
 *     z-index counter, so the overlay opened last is on top; one
 *     scroll-lock counter (body.ah-scroll-locked); one stack of open
 *     overlays: Escape closes the top one (after the escHooks, e.g. open
 *     tooltips, had their turn), Tab is trapped in the top one when it is
 *     modal, focus returns to the element that had it when the overlay
 *     closes
 *   - the declarative triggers (aihtml_lib_overlay:opens/toggles/closes),
 *     one delegated click listener: data-ah-open / data-ah-toggle /
 *     data-ah-close hold a selector; an empty data-ah-close closes the
 *     enclosing overlay
 *   - bubble positioning with an arrow (ArrowFloat: tooltip, popover)
 *   - opacity fades (Fader, one instance: fades)
 *   - the drawer / sheet controller (SlideController)
 *   - notification cards in a screen corner (NotifyCard: toast,
 *     notification)
 *
 * Events (native CustomEvents) on the component root: ah:opening and
 * ah:closing (cancelable; the window's ah:closing has detail
 * OverlayClose), ah:open, ah:close (detail OverlayClose), and for the
 * window ah:collapse, ah:expand, ah:moving / ah:moved (detail
 * WindowPosition), ah:resize (detail WindowSize); notification cards fire
 * ah:click. */
// ah-load: [data-ah-open], [data-ah-toggle], [data-ah-close]
import AH from "../core.ts";
import type { FloatHandle, Placement } from "../core.ts";
import { visible } from "./_lib_nav.ts";

/** Detail of ah:close (and of the window's ah:closing): the result the
 *  closing control carried (data-ah-result) or the server passed to
 *  close, null when none. */
export interface OverlayClose { result: unknown; }

/** What open / toggle take from a trigger: the element that opened it. */
export interface OpenOptions { invoker?: Element | null; }

export const FOCUSABLE = "a[href], area[href], button:not([disabled]), " +
  "input:not([disabled]):not([type='hidden']), select:not([disabled]), " +
  "textarea:not([disabled]), iframe, [tabindex]:not([tabindex='-1']), " +
  "[contenteditable='true']";
export const OVERLAYS = '[data-ah="drawer"],[data-ah="sheet"],[data-ah="window"],' +
  '[data-ah="popover"],[data-ah="tooltip"],[data-ah="notification"]';

const PLACEMENTS: readonly Placement[] = ["bottom", "top", "right", "left"];

/** A placement name, or dflt when s is not one. */
export function placement(s: string | null, dflt: Placement): Placement {
  const hit = PLACEMENTS.find((p) => p === s);
  return hit || dflt;
}

/** data-ah-<name>: "false" is false, absent is the default. */
export function flag(el: Element, name: string, dflt: boolean): boolean {
  const v = el.getAttribute("data-ah-" + name);
  if (v === null || v === "") { return dflt; }
  return v !== "false";
}

/** data-ah-<name> as a number, dflt when absent or not a number. */
export function num(el: Element, name: string, dflt: number): number {
  const v = parseFloat(el.getAttribute("data-ah-" + name) || "");
  return isNaN(v) ? dflt : v;
}

/** A native bubbling, cancelable event; false when a listener prevented
 *  it. */
export function fire<D>(target: EventTarget, type: string, detail?: D): boolean {
  return target.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

/** The elements from node up to the document that match selector
 *  (innermost first): what a delegated handler runs for. Throws on a bad
 *  selector. */
export function matching(node: EventTarget | null, selector: string): Element[] {
  const out: Element[] = [];
  let n: Element | null = node instanceof Element ? node : node instanceof Node ? node.parentElement : null;
  for (; n; n = n.parentElement) {
    if (n.matches(selector)) { out.push(n); }
  }
  return out;
}

/** The closest ancestor-or-self of an event target matching selector. */
export function closest(node: EventTarget | null, selector: string): Element | null {
  return node instanceof Element ? node.closest(selector) : null;
}

/** Focus el when it can take focus (an HTML or SVG element). */
export function focusEl(el: Element | null, opts?: FocusOptions): void {
  if (el instanceof HTMLElement || el instanceof SVGElement) {
    try { el.focus(opts); } catch { /* not focusable */ }
  }
}

// ------------------------------------------------------------------
// Fades: one running opacity animation per element, a new one
// replaces it.
// ------------------------------------------------------------------

interface Fade { anim: Animation; end: () => void; }

export class Fader {
  readonly #running = new WeakMap<HTMLElement, Fade>();

  /** Stop the element's fade: where it is (the opacity it reached stays
   *  inline), or at its end (jumpToEnd: its completion runs now). */
  stop(el: HTMLElement, jumpToEnd: boolean): void {
    const f = this.#running.get(el);
    if (!f) { return; }
    this.#running.delete(el);
    f.anim.onfinish = null;
    if (!jumpToEnd) { el.style.opacity = window.getComputedStyle(el).opacity; }
    f.anim.cancel();
    if (jumpToEnd) { f.end(); }
  }

  /** Fade el's opacity to `to' in ms, then done(). The final opacity
   *  stays inline unless end() changes it. */
  fade(el: HTMLElement, to: number, ms: number, done?: () => void, end?: () => void): void {
    this.stop(el, false);
    const from = parseFloat(window.getComputedStyle(el).opacity);
    const finish = (): void => {
      el.style.opacity = String(to);
      if (end) { end(); }
      if (done) { done(); }
    };
    if (!ms || typeof el.animate !== "function") {
      finish();
      return;
    }
    el.style.opacity = String(to);
    const anim = el.animate([{ opacity: isNaN(from) ? 1 : from }, { opacity: to }],
                            { duration: ms, easing: "ease-in-out" });
    const f: Fade = { anim: anim, end: finish };
    this.#running.set(el, f);
    anim.onfinish = () => {
      if (this.#running.get(el) === f) { this.#running.delete(el); }
      finish();
    };
  }

  /** Show a hidden element (its stylesheet display, block if that hides
   *  it) and fade it in. */
  fadeIn(el: HTMLElement, ms: number, done?: () => void): void {
    this.stop(el, false);
    if (window.getComputedStyle(el).display === "none") {
      el.style.display = "";
      if (window.getComputedStyle(el).display === "none") { el.style.display = "block"; }
      el.style.opacity = "0";
    }
    this.fade(el, 1, ms, done, () => { el.style.opacity = ""; });
  }

  /** Fade out, then display: none (opacity restored for the next show). */
  fadeOut(el: HTMLElement, ms: number, done?: () => void): void {
    if (window.getComputedStyle(el).display === "none") {
      this.stop(el, false);
      el.style.display = "none";
      if (done) { done(); }
      return;
    }
    this.fade(el, 0, ms, done, () => {
      el.style.display = "none";
      el.style.opacity = "";
    });
  }
}

export const fades = new Fader();

// ------------------------------------------------------------------
// The overlay layer: z-index, scroll lock, stack of open overlays
// ------------------------------------------------------------------

/** An open overlay: Tab stays in trap (when set); esc(e) handles Escape
 *  when it is on top and returns whether it did. */
export interface StackEntry {
  el: Element;
  trap: HTMLElement | null;
  esc: (e: KeyboardEvent) => boolean;
}

interface OpenEntry extends StackEntry { returnTo: Element | null; }

export class OverlayLayer {
  /** Run on Escape before the stack; one returning true handled it. */
  readonly escHooks: (() => boolean)[] = [];
  // Above sigil's fixed drawer (20600) and sheet (20500) layers, below
  // the notification corner (99999).
  #z = 21000;
  #locks = 0;
  #seq = 0;
  #stack: OpenEntry[] = [];

  constructor() {
    document.addEventListener("keydown", (e) => { this.#keydown(e); });
  }

  /** A fresh id: prefix + a sequence number. */
  uid(prefix: string): string { return prefix + (++this.#seq); }

  nextZ(): number { return ++this.#z; }

  lock(): void {
    if (this.#locks++ === 0) { document.body.classList.add("ah-scroll-locked"); }
  }

  unlock(): void {
    this.#locks = Math.max(0, this.#locks - 1);
    if (this.#locks === 0) { document.body.classList.remove("ah-scroll-locked"); }
  }

  /** Put an overlay on top; focus returns to the current element when it
   *  is pulled with restore. */
  push(entry: StackEntry): void {
    this.pull(entry.el, false);
    this.#stack.push({ ...entry, returnTo: document.activeElement });
  }

  pull(el: Element, restore: boolean): void {
    const kept: OpenEntry[] = [];
    let gone: OpenEntry | null = null;
    for (const s of this.#stack) {
      if (s.el === el) { gone = s; } else { kept.push(s); }
    }
    this.#stack = kept;
    const back = gone ? gone.returnTo : null;
    if (restore && back && document.contains(back)) {
      const active = document.activeElement;
      if (!active || active === document.body || el.contains(active)) {
        focusEl(back, { preventScroll: true });
      }
    }
  }

  top(): StackEntry | undefined { return this.#stack[this.#stack.length - 1]; }

  /** The visible focusable elements inside container. */
  static focusables(container: Element): HTMLElement[] {
    return Array.from(container.querySelectorAll<HTMLElement>(FOCUSABLE)).filter(visible);
  }

  static trapTab(e: KeyboardEvent, container: HTMLElement): void {
    const f = OverlayLayer.focusables(container);
    const active = document.activeElement;
    if (!f.length) {
      e.preventDefault();
      container.focus();
      return;
    }
    const first = f[0];
    const last = f[f.length - 1];
    if (!container.contains(active)) {
      e.preventDefault();
      first.focus();
    } else if (e.shiftKey && (active === first || active === container)) {
      e.preventDefault();
      last.focus();
    } else if (!e.shiftKey && active === last) {
      e.preventDefault();
      first.focus();
    }
  }

  #keydown(e: KeyboardEvent): void {
    if (e.key === "Escape") {
      for (const hook of this.escHooks) {
        if (hook()) { return; }
      }
      const t = this.top();
      if (t && t.esc(e)) { e.preventDefault(); }
    } else if (e.key === "Tab") {
      const top = this.top();
      if (top && top.trap) { OverlayLayer.trapTab(e, top.trap); }
    }
  }
}

export const overlays = new OverlayLayer();

// ------------------------------------------------------------------
// Declarative triggers
// ------------------------------------------------------------------

function each(sel: string, f: (el: Element) => void): void {
  let els: NodeListOf<Element>;
  try { els = document.querySelectorAll(sel); } catch {
    console.error("aihtml: bad selector " + sel);
    return;
  }
  els.forEach(f);
}

document.addEventListener("click", (e) => {
  const triggers = matching(e.target, "[data-ah-open],[data-ah-toggle],[data-ah-close]");
  for (let i = 0; i < triggers.length && !e.cancelBubble; i++) {
    const trigger = triggers[i];
    if (trigger.tagName === "A") { e.preventDefault(); }
    const open = trigger.getAttribute("data-ah-open");
    if (open) {
      each(open, (t) => { AH.invoke(t, "open", { invoker: trigger }); });
    }
    const toggle = trigger.getAttribute("data-ah-toggle");
    if (toggle) {
      each(toggle, (t) => { AH.invoke(t, "toggle", { invoker: trigger }); });
    }
    if (trigger.hasAttribute("data-ah-close")) {
      const sel = trigger.getAttribute("data-ah-close");
      const result = trigger.getAttribute("data-ah-result");
      if (sel) {
        each(sel, (t) => { AH.invoke(t, "close", result); });
      } else {
        const t = trigger.closest(OVERLAYS);
        if (t) { AH.invoke(t, "close", result); }
      }
    }
  }
});

// Enter / Space on non-button close controls (popover title bar).
document.addEventListener("keydown", (e) => {
  if (e.key !== "Enter" && e.key !== " ") { return; }
  const c = closest(e.target, "[data-ah-close][role='button']");
  if (c instanceof HTMLElement) {
    e.preventDefault();
    c.click();
  }
});

// ------------------------------------------------------------------
// Positioning: AH.float (core.ts) places the bubble with position:
// fixed, flips it and follows scrolling; the side it used lands in
// data-ah-placement, which is mirrored into sigil's arrow class
// (<prefix><side>) whenever it changes. sideClasses: the classes to
// remove, space separated.
// ------------------------------------------------------------------

export class ArrowFloat implements FloatHandle {
  readonly #el: HTMLElement;
  readonly #prefix: string;
  readonly #remove: string[];
  readonly #handle: FloatHandle;
  readonly #observer: MutationObserver | null;

  constructor(el: HTMLElement, anchor: Element, side: Placement, offset: number,
              prefix: string, sideClasses: string) {
    this.#el = el;
    this.#prefix = prefix;
    this.#remove = sideClasses.split(/\s+/).filter(Boolean);
    this.#handle = AH.float(el, anchor, { placement: side, align: "center", offset: offset });
    this.#sync();
    this.#observer = window.MutationObserver ? new MutationObserver(() => { this.#sync(); }) : null;
    if (this.#observer) {
      this.#observer.observe(el, { attributes: true, attributeFilter: ["data-ah-placement"] });
    }
  }

  update(): void { this.#handle.update(); }

  stop(): void {
    if (this.#observer) { this.#observer.disconnect(); }
    this.#handle.stop();
  }

  #sync(): void {
    const used = this.#el.getAttribute("data-ah-placement");
    if (used) {
      this.#el.classList.remove(...this.#remove);
      this.#el.classList.add(this.#prefix + used);
    }
  }
}

// ------------------------------------------------------------------
// Drawer and sheet: the root is the scrim (…__overlay), CSS animates
// data-state; the drawer adds sigil's swipe-to-dismiss gesture.
// ------------------------------------------------------------------

type Side = "right" | "left" | "bottom" | "top";

interface DragAxis { prop: "translateX" | "translateY"; sign: 1 | -1; dim: "w" | "h"; }

const DRAG_AXIS: Record<Side, DragAxis> = {
  right: { prop: "translateX", sign: 1, dim: "w" },
  left: { prop: "translateX", sign: -1, dim: "w" },
  bottom: { prop: "translateY", sign: 1, dim: "h" },
  top: { prop: "translateY", sign: -1, dim: "h" }
};

function side(s: string | null): Side {
  return s === "right" || s === "left" || s === "top" ? s : "bottom";
}

/** A swipe in progress: the panel's side and size, where and when it
 *  started, how far it has moved towards closing. */
interface SlideDrag { side: Side; size: number; x0: number; y0: number; t0: number; d: number; }

/** The drawer and sheet behaviour; the subclass names the component. */
export abstract class SlideController extends AH.Controller {
  /** The component: its class prefix is ah-<kind>. */
  protected abstract readonly kind: "drawer" | "sheet";
  #drag: SlideDrag | null = null;

  override setup(): void {
    const el = this.element;
    this.listen(el, "mousedown", (e) => {
      if (e.target === el && flag(el, "scrim", true)) { this.close(); }
    });
    if (this.kind === "drawer") { this.#listenDrag(); }
    if (el.getAttribute("data-ah-initial") === "open") { this.open(); }
  }

  override teardown(): void {
    this.#drag = null;
    if (this.isOpen()) {
      overlays.unlock();
      overlays.pull(this.element, false);
    }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void {
    const el = this.element;
    if (this.isOpen()) { return; }
    if (!this.fire("ah:opening")) { return; }
    el.style.zIndex = String(overlays.nextZ());
    void el.offsetHeight;             // first open: let the transition run
    this.#setState("open");
    overlays.lock();
    const p = this.#panel;
    overlays.push({
      el: el, trap: p,
      esc: () => {
        if (!flag(el, "esc", true)) { return false; }
        this.close();
        return true;
      }
    });
    if (p) { p.focus({ preventScroll: true }); }
    this.fire("ah:open");
  }

  close(result?: unknown): void {
    if (!this.isOpen()) { return; }
    if (!this.fire("ah:closing")) { return; }
    const p = this.#panel;
    if (p) {
      p.style.transform = "";
      p.setAttribute("data-dragging", "false");
    }
    this.#setState("closed");
    overlays.unlock();
    overlays.pull(this.element, true);
    this.fire<OverlayClose>("ah:close", { result: result || null });
  }

  toggle(): void { if (this.isOpen()) { this.close(); } else { this.open(); } }

  isOpen(): boolean { return this.element.getAttribute("data-state") === "open"; }

  get #panel(): HTMLElement | null {
    return this.element.querySelector<HTMLElement>(".ah-" + this.kind + "__panel");
  }

  #setState(s: "open" | "closed"): void {
    this.element.setAttribute("data-state", s);
    const p = this.#panel;
    if (p) { p.setAttribute("data-state", s); }
  }

  #listenDrag(): void {
    const el = this.element;
    this.listen(el, "pointerdown", (e) => {
      const p = this.#panel;
      const t = e.target;
      if (!p || !flag(el, "dismissible", true) || !(t instanceof Element) || !p.contains(t) ||
          t.closest("button, a, input, textarea, select, .ah-" + this.kind + "__body")) {
        return;
      }
      const sd = side(p.getAttribute("data-side"));
      const r = p.getBoundingClientRect();
      this.#drag = { side: sd, size: DRAG_AXIS[sd].dim === "w" ? r.width : r.height,
                     x0: e.clientX, y0: e.clientY, t0: e.timeStamp, d: 0 };
      p.setAttribute("data-dragging", "true");
      try { p.setPointerCapture(e.pointerId); } catch { /* no capture */ }
    });
    this.listen(el, "pointermove", (e) => {
      const st = this.#drag;
      const p = this.#panel;
      if (!st || !p) { return; }
      const ax = DRAG_AXIS[st.side];
      const raw = ax.dim === "w" ? e.clientX - st.x0 : e.clientY - st.y0;
      st.d = Math.max(0, ax.sign * raw);   // only towards closing
      p.style.transform = ax.prop + "(" + (ax.sign * st.d) + "px)";
    });
    const up = (e: PointerEvent): void => {
      const s = this.#drag;
      if (!s) { return; }
      this.#drag = null;
      const p = this.#panel;
      const dt = Math.max(1, e.timeStamp - s.t0);
      if (p) { p.setAttribute("data-dragging", "false"); }
      // past 30% of the panel, or a flick faster than 0.5px/ms that
      // also moved 50px (a short tap must not count as a flick)
      if (s.d > s.size * 0.3 || (s.d / dt > 0.5 && s.d > 50)) {
        this.close();
      } else if (p) {
        p.style.transform = "";
      }
    };
    this.listen(el, "pointerup", up);
    this.listen(el, "pointercancel", up);
  }
}

// ------------------------------------------------------------------
// Notification cards, toast (sigil overlay/notification + toast)
// ------------------------------------------------------------------

// Card markup comes from templates/notification.mustache (toast content
// from templates/toast.mustache), the same templates aihtml_lib_overlay
// renders on the server. Only the corner container is built here.
type Variant = "info" | "success" | "warning" | "error";
type Corner = "top-right" | "top-left" | "bottom-right" | "bottom-left";

const VARIANTS: readonly Variant[] = ["info", "success", "warning", "error"];
const CORNERS: readonly Corner[] = ["top-right", "top-left", "bottom-right", "bottom-left"];

function corner(pos: unknown): Corner {
  return CORNERS.find((c) => c === pos) || "top-right";
}

/** The card options toast and notify share (sigil's props). */
export interface CardOptions {
  variant?: string | null;
  closable?: boolean | string | null;
  closeOnClick?: boolean | string | null;
  /** a number is pixels */
  width?: number | string | null;
}

/** The view of templates/notification.mustache. */
export interface CardView {
  variant: Variant;
  info: boolean;
  success: boolean;
  warning: boolean;
  error: boolean;
  clickable: boolean;
  closable: boolean;
  width: string | null;
  content: string;
}

/** The view for templates/notification.mustache; aihtml_lib_overlay:card/2
 *  builds the same one. contentHtml must be trusted HTML. */
export function cardView(o: CardOptions, contentHtml: string): CardView {
  const v = VARIANTS.find((x) => x === o.variant) || "info";
  const w = o.width;
  return {
    variant: v, info: v === "info", success: v === "success",
    warning: v === "warning", error: v === "error",
    clickable: o.closeOnClick !== false && o.closeOnClick !== "false",
    closable: o.closable !== false && o.closable !== "false",
    width: w === undefined || w === null || w === "" ? null
      : (typeof w === "number" ? w + "px" : String(w)),
    content: contentHtml
  };
}

/** A duration option in ms: dflt when absent. */
export function duration(v: unknown, dflt: number): number {
  return v === undefined || v === null || v === "" ? dflt : Number(v);
}

const ESCAPES: Record<string, string> = { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;" };

/** HTML-escape text (for content put into a card). */
export function escapeHtml(s: unknown): string {
  return String(s).replace(/[&<>"']/g, (c) => ESCAPES[c] || c);
}

/** Where and how long a card shows: duration in ms (<= 0 stays); events
 *  go to source (a notification template) or the card. */
export interface ShowOptions {
  position?: string | null;
  duration: number;
  source?: Element;
}

/** A card shown in its screen corner, until it expires or is closed. */
export class NotifyCard {
  static readonly #shown = new WeakMap<Element, NotifyCard>();

  readonly #card: HTMLElement;
  readonly #container: HTMLElement;
  readonly #target: Element;
  readonly #duration: number;
  #timer: ReturnType<typeof setTimeout> | undefined;
  #closed = false;

  /** Put a card (trusted HTML string or element) in its corner and run
   *  it. Whether a click on the card closes it is read from the markup
   *  (.ah-notify-clickable). Returns the card element. */
  static show(card: string | HTMLElement, o: ShowOptions): HTMLElement | undefined {
    let el: HTMLElement | undefined;
    if (typeof card === "string") {
      const t = document.createElement("template");
      t.innerHTML = card;
      el = Array.from(t.content.children).find(
        (n): n is HTMLElement => n instanceof HTMLElement && n.classList.contains("ah-notify"));
    } else {
      el = card;
    }
    if (!el) { return undefined; }
    return new NotifyCard(el, o).#card;
  }

  /** Close a card shown by show (fades it out). */
  static close(card: Element): void {
    const c = NotifyCard.#shown.get(card);
    if (c) { c.close(); }
  }

  /** The corner container, created on first use. */
  static corner(pos: unknown): HTMLElement {
    const p = corner(pos);
    let c = document.querySelector<HTMLElement>("body > .ah-notify-container.ah-notify-" + p);
    if (!c) {
      c = document.createElement("div");
      c.className = "ah-notify-container ah-notify-" + p;
      document.body.appendChild(c);
    }
    return c;
  }

  private constructor(card: HTMLElement, o: ShowOptions) {
    const pos = corner(o.position);
    const c = NotifyCard.corner(pos);
    if (pos.indexOf("bottom") === 0) { c.insertBefore(card, c.firstChild); } else { c.appendChild(card); }
    this.#card = card;
    this.#container = c;
    this.#target = o.source || card;
    this.#duration = o.duration;
    NotifyCard.#shown.set(card, this);
    card.addEventListener("click", (e) => {
      if (closest(e.target, ".ah-notify-close")) {
        e.stopPropagation();
        this.close();
        return;
      }
      if (card.classList.contains("ah-notify-clickable")) {
        fire(this.#target, "ah:click");
        this.close();
      }
    });
    card.addEventListener("keydown", (e) => {
      if ((e.key === "Enter" || e.key === " ") && closest(e.target, ".ah-notify-close")) {
        e.preventDefault();
        this.close();
      }
    });
    // hovering keeps the card (sigil lets it expire under the pointer)
    card.addEventListener("mouseenter", () => { clearTimeout(this.#timer); });
    card.addEventListener("mouseleave", () => { if (!this.#closed) { this.#arm(); } });
    this.#arm();
    card.style.display = "flex";
    card.style.opacity = "0";
    fades.fade(card, 0.95, 300, () => {
      card.style.opacity = "";
      fire(this.#target, "ah:open");
    });
  }

  close(): void {
    if (this.#closed) { return; }
    this.#closed = true;
    clearTimeout(this.#timer);
    NotifyCard.#shown.delete(this.#card);
    fades.fadeOut(this.#card, 300, () => {
      this.#card.remove();
      if (!this.#container.children.length) { this.#container.remove(); }
      fire<OverlayClose>(this.#target, "ah:close", { result: null });
    });
  }

  #arm(): void {
    if (this.#duration > 0) { this.#timer = setTimeout(() => { this.close(); }, this.#duration); }
  }
}
