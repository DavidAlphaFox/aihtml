/* The tooltip behaviour (designs/04-components.md), ported from sigil's
 * overlay/tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip]
 * elements (tooltip_attrs/2), driven by delegated document listeners
 * (Tooltips). Escape closes open tooltips before any other overlay (an
 * escHook of the overlay layer, _lib_overlay.ts). Events on the host:
 * ah:opening (cancelable), ah:open, ah:close (detail OverlayClose,
 * {result: null}). */
// ah-load: [data-ah-tooltip]
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { ArrowFloat, fades, fire, matching, overlays, placement } from "./_lib_overlay.ts";
import type { OverlayClose } from "./_lib_overlay.ts";

export type { OverlayClose } from "./_lib_overlay.ts";

// ------------------------------------------------------------------
// Tooltip: [data-ah="tooltip"] wrappers and [data-ah-tooltip] elements
// ------------------------------------------------------------------

const TIP_HOSTS = '[data-ah="tooltip"],[data-ah-tooltip]';
const TIP_POSITIONS = "ah-tooltip-top ah-tooltip-bottom ah-tooltip-left ah-tooltip-right";

/** Where the pointer was (for position "mouse"). */
interface Point { clientX: number; clientY: number; }

/** The tooltip of one host: its bubble (the wrapper's own, or one made
 *  for a [data-ah-tooltip] element), timers and float. */
class Tip {
  readonly host: Element;
  isOpen = false;
  showTimer: ReturnType<typeof setTimeout> | undefined;
  hideTimer: ReturnType<typeof setTimeout> | undefined;
  #tip: HTMLElement | null = null;
  #created = false;
  #float: FloatHandle | null = null;
  readonly #tips: Tooltips;

  constructor(host: Element, tips: Tooltips) {
    this.host = host;
    this.#tips = tips;
  }

  /** data-ah-tip-<name>, dflt when absent or empty. */
  opt(name: string, dflt: string): string {
    const v = this.host.getAttribute("data-ah-tip-" + name);
    return v === null || v === "" ? dflt : v;
  }

  /** The bubble element, while it exists. */
  get bubble(): HTMLElement | null { return this.#tip; }

  /** What opens it: hover (click on touch-only screens), click, ... */
  get trigger(): string {
    const t = this.opt("trigger", "hover");
    return t === "hover" && this.#tips.touchOnly ? "click" : t;
  }

  show(e?: Point): void {
    const host = this.host;
    if (this.isOpen || this.opt("disabled", "false") === "true") { return; }
    if (!fire(host, "ah:opening")) { return; }
    const tip = this.#element();
    if (!tip) { return; }
    fades.stop(tip, false);
    Object.assign(tip.style, { display: "block", visibility: "hidden", opacity: "0" });
    this.#position(tip, e);
    tip.style.visibility = "visible";
    fades.fade(tip, 0.9, 200);
    host.setAttribute("aria-describedby", tip.id);
    this.isOpen = true;
    this.#tips.opened(this);
    fire(host, "ah:open");
    if (this.opt("auto-hide", "true") !== "false") {
      clearTimeout(this.hideTimer);
      this.hideTimer = setTimeout(() => { this.hide(); },
                                  parseInt(this.opt("hide-delay", "3000"), 10));
    }
  }

  /** Close it: faded, or at once (now). */
  hide(now?: boolean): void {
    clearTimeout(this.showTimer);
    clearTimeout(this.hideTimer);
    if (!this.isOpen) { return; }
    this.isOpen = false;
    this.#tips.closed(this);
    this.host.removeAttribute("aria-describedby");
    const tip = this.#tip;
    const done = (): void => {
      this.#unfloat();
      if (!tip) { return; }
      tip.style.display = "none";
      tip.style.visibility = "hidden";
      if (this.#created) {
        tip.remove();
        this.#tip = null;
      }
    };
    if (!tip) {
      done();
    } else if (now) {
      fades.stop(tip, false);
      done();
    } else {
      fades.fade(tip, 0, 200, done);
    }
    fire<OverlayClose>(this.host, "ah:close", { result: null });
  }

  toggle(e?: Point): void {
    if (this.isOpen) { this.hide(); } else { this.show(e); }
  }

  #element(): HTMLElement | null {
    const host = this.host;
    if (this.#tip) { return this.#tip; }
    if (host.getAttribute("data-ah") === "tooltip") {
      this.#tip = host.querySelector<HTMLElement>(":scope > .ah-tooltip");
    } else {
      const tip = document.createElement("span");
      tip.className = "ah-tooltip";
      tip.setAttribute("role", "tooltip");
      tip.innerHTML = '<span class="ah-tooltip-arrow" aria-hidden="true"></span>' +
        '<span class="ah-tooltip-content"></span>';
      const content = tip.querySelector(".ah-tooltip-content");
      if (content) { content.textContent = host.getAttribute("data-ah-tooltip"); }
      if (this.opt("arrow", "true") === "false") { tip.classList.add("ah-tooltip-no-arrow"); }
      document.body.appendChild(tip);
      this.#tip = tip;
      this.#created = true;
    }
    if (this.#tip && !this.#tip.id) { this.#tip.id = overlays.uid("ah-tip-"); }
    return this.#tip;
  }

  #position(tip: HTMLElement, e?: Point): void {
    const host = this.host;
    const pos = this.opt("position", "bottom");
    this.#unfloat();
    tip.style.position = "fixed";
    if (pos === "mouse") {
      tip.classList.add("ah-tooltip-no-arrow");
      const x = e && e.clientX !== undefined ? e.clientX : host.getBoundingClientRect().left;
      const y = e && e.clientY !== undefined ? e.clientY : host.getBoundingClientRect().bottom;
      tip.style.left = (x + 10) + "px";
      tip.style.top = (y + 10) + "px";
      return;
    }
    // the tooltip's arrow is 6px (tooltip.css)
    let anchor: Element = host;
    if (host.getAttribute("data-ah") === "tooltip") {
      anchor = Array.from(host.children).find((c) => !c.classList.contains("ah-tooltip")) || host;
    }
    this.#float = new ArrowFloat(tip, anchor, placement(pos, "bottom"), 6, "ah-tooltip-", TIP_POSITIONS);
  }

  #unfloat(): void {
    if (this.#float) {
      this.#float.stop();
      this.#float = null;
    }
  }
}

/** Every tooltip of the page: the Tip of each host, the open ones, and
 *  the delegated document listeners that drive them. */
class Tooltips {
  readonly touchOnly = !!(window.matchMedia && window.matchMedia("(hover: none)").matches);
  readonly #tips = new WeakMap<Element, Tip>();
  #open: Tip[] = [];

  constructor() {
    // mouseenter / mouseleave of each host, from the bubbling mouseover /
    // mouseout (the pointer came from, or went to, outside the host)
    document.addEventListener("mouseover", (e) => {
      this.#hosts(e.target).forEach((host) => {
        if (Tooltips.inside(host, e.relatedTarget)) { return; }
        const t = this.of(host);
        if (t.trigger !== "hover") { return; }
        clearTimeout(t.showTimer);
        const ev: Point = { clientX: e.clientX, clientY: e.clientY };
        t.showTimer = setTimeout(() => {
          if (document.contains(host)) { t.show(ev); }
        }, parseInt(t.opt("delay", "100"), 10));
      });
    });
    document.addEventListener("mouseout", (e) => {
      this.#hosts(e.target).forEach((host) => {
        if (Tooltips.inside(host, e.relatedTarget)) { return; }
        const t = this.of(host);
        const active = document.activeElement;
        if (t.trigger === "hover" && !(host !== active && host.contains(active))) {
          t.hide();
        }
      });
    });
    document.addEventListener("mousemove", (e) => {
      this.#hosts(e.target).forEach((host) => {
        const t = this.#tips.get(host);
        const tip = t ? t.bubble : null;
        if (t && t.isOpen && tip && t.opt("position", "") === "mouse") {
          tip.style.left = (e.clientX + 10) + "px";
          tip.style.top = (e.clientY + 10) + "px";
        }
      });
    });
    document.addEventListener("focusin", (e) => {
      this.#hosts(e.target).forEach((host) => {
        const t = this.of(host);
        if (t.trigger === "hover") { t.show(); }
      });
    });
    document.addEventListener("focusout", (e) => {
      this.#hosts(e.target).forEach((host) => {
        const to = e.relatedTarget;
        const t = this.of(host);
        if (t.trigger === "hover" && !(to !== host && Tooltips.inside(host, to))) {
          t.hide();
        }
      });
    });
    document.addEventListener("click", (e) => {
      const inner = this.#hosts(e.target)[0];
      if (inner) {
        const t = this.of(inner);
        if (t.trigger === "click") { t.toggle(e); }
      }
      // click-triggered tooltips close on a click elsewhere
      this.#open.slice().forEach((t) => {
        if (t.trigger === "click" && !Tooltips.inside(t.host, e.target)) { t.hide(); }
      });
    });
    overlays.escHooks.push(() => this.closeAll());
  }

  /** The Tip of host (made on first use). */
  of(host: Element): Tip {
    let t = this.#tips.get(host);
    if (!t) {
      t = new Tip(host, this);
      this.#tips.set(host, t);
    }
    return t;
  }

  /** Close host's tooltip at once and forget it. */
  forget(host: Element): void {
    const t = this.#tips.get(host);
    if (t) { t.hide(true); }
    this.#tips.delete(host);
  }

  opened(t: Tip): void { this.#open.push(t); }
  closed(t: Tip): void { this.#open = this.#open.filter((x) => x !== t); }

  /** Close the open tooltips; whether there were any (the escHook). */
  closeAll(): boolean {
    const had = this.#open.length > 0;
    this.#open.slice().forEach((t) => { t.hide(); });
    return had;
  }

  static inside(host: Element, node: EventTarget | null): boolean {
    return node instanceof Node && host.contains(node);
  }

  #hosts(node: EventTarget | null): Element[] { return matching(node, TIP_HOSTS); }
}

const tooltips = new Tooltips();

class TooltipController extends AH.Controller {
  // the delegated document listeners of Tooltips do the work
  override teardown(): void { tooltips.forget(this.element); }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void { tooltips.of(this.element).show(); }
  close(): void { tooltips.of(this.element).hide(); }
  toggle(): void { tooltips.of(this.element).toggle(); }
  setContent(text: unknown): void {
    this.element.querySelectorAll(".ah-tooltip-content").forEach((c) => {
      c.textContent = text === null || text === undefined ? "" : String(text);
    });
  }
}

AH.register("tooltip", TooltipController);
