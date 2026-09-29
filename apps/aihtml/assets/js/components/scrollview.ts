/* Behaviour of the scrollview component (designs/04-components.md),
 * ported from sigil's scrollview (cljs + jQuery). The value is kept in
 * data-ah-value on the root, mirrored into a hidden input when there is
 * one, and "change" fires on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change". Drags use pointer capture, so nothing is bound
 * on document. ah:page-changed fires on the root when the page changes,
 * detail ScrollviewPageChanged {page, old}.
 */
import AH from "../core.ts";
import { capture, num, setValue } from "./_lib_scroll.ts";

/** Detail of ah:page-changed. */
export interface ScrollviewPageChanged { page: number; old: number; }

// ------------------------------------------------------------------
// ScrollView: a horizontal pager (sigil layout/scrollview)
// ------------------------------------------------------------------
//
// Pages are 100% wide and the wrapper moves by margin-left in percent,
// so the layout follows the container without measuring; drags move it
// in pixels and the page change animates back to a percentage.

const SV = "ah-scrollview";
const SV_DEAD_ZONE = 15;

/** "user" fires change, "api" and "auto" only ah:page-changed. */
type How = "user" | "api" | "auto";

interface Drag { x: number; ml: number; moving: boolean; e: PointerEvent; }

// content width (jQuery's .width())
function contentWidth(el: Element): number {
  const cs = getComputedStyle(el);
  return el.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0);
}

class ScrollviewController extends AH.Controller {
  #timer: ReturnType<typeof setInterval> | null = null;
  #anim: ReturnType<typeof setTimeout> | null = null;
  #paused = false;
  #drag: Drag | null = null;

  override setup(): void {
    const el = this.element;
    this.#timer = null;
    this.#anim = null;
    this.#paused = false;
    this.#drag = null;
    // the server always renders the wrapper
    const w = this.wrapper() as HTMLElement;
    this.mark(this.index());

    this.listen(w, "pointerdown", (e) => {
      if (this.disabled() || (e.button !== undefined && e.button !== 0)) { return; }
      el.classList.remove(SV + "-animating");
      this.#drag = { x: e.clientX, ml: parseFloat(getComputedStyle(w).marginLeft) || 0, moving: false, e };
    });
    this.listen(w, "pointermove", (e) => {
      const drag = this.#drag;
      if (!drag) { return; }
      const dx = e.clientX - drag.x;
      if (!drag.moving) {
        if (Math.abs(dx) <= SV_DEAD_ZONE) { return; }
        drag.moving = true;
        capture(w, drag.e);
        el.classList.add(SV + "-dragging");
      }
      e.preventDefault();
      const cw = contentWidth(el), cnt = this.pages().length;
      let ml = drag.ml + dx;
      if (el.getAttribute("data-bounce") === "false") {
        ml = Math.max(-(cnt - 1) * cw, Math.min(0, ml));
      }
      w.style.marginLeft = ml + "px";
    });
    const end = (): void => {
      const d = this.#drag;
      if (!d) { return; }
      this.#drag = null;
      if (!d.moving) { return; }
      el.classList.remove(SV + "-dragging");
      // swallow the click that ends a drag, so links in a page stay put
      const swallow = (c: Event): void => { c.preventDefault(); c.stopPropagation(); };
      w.addEventListener("click", swallow, { once: true, capture: true });
      setTimeout(() => { w.removeEventListener("click", swallow, { capture: true }); }, 0);
      const dx = (parseFloat(w.style.marginLeft) || 0) - d.ml;
      const cw = contentWidth(el), threshold = num(el, "data-threshold", 0.5) * cw;
      const cur = this.index();
      let target = cur;
      if (dx < -threshold) { target = cur + 1; } else if (dx > threshold) { target = cur - 1; }
      this.go(target, "user");
    };
    this.listen(w, "pointerup", end);
    this.listen(w, "pointercancel", end);
    this.listen(w, "dragstart", (e) => { e.preventDefault(); });

    this.delegate("click", "." + SV + "-button", (_e, b) => {
      if (b.closest("." + SV) !== el || this.disabled() || !b.parentNode) { return; }
      this.go(Array.prototype.indexOf.call(b.parentNode.children, b), "user");
    });

    this.listen(el, "keydown", (e) => {
      if (e.target !== el || this.disabled()) { return; }
      const cur = this.index(), cnt = this.pages().length;
      let to: number;
      switch (e.key) {
        case "ArrowLeft": case "ArrowUp": case "PageUp": to = cur - 1; break;
        case "ArrowRight": case "ArrowDown": case "PageDown": to = cur + 1; break;
        case "Home": to = 0; break;
        case "End": to = cnt - 1; break;
        default: return;
      }
      e.preventDefault();
      this.go(to, "user");
    });

    // the slide show waits while the pointer or the focus is inside
    const pause = (): void => { this.#paused = true; };
    this.listen(el, "mouseenter", pause);
    this.listen(el, "focusin", pause);
    this.listen(el, "mouseleave", () => { this.#paused = false; });
    this.listen(el, "focusout", (e) => {
      if (e.relatedTarget instanceof Node && el.contains(e.relatedTarget)) { return; }
      this.#paused = false;
    });
    if (el.hasAttribute("data-slide-show")) { this.startSlideShow(); }
  }

  override teardown(): void {
    this.stopSlideShow();
    if (this.#anim !== null) { clearTimeout(this.#anim); }
    this.#anim = null;
    this.element.classList.remove(SV + "-animating");
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(v: number | string): void { this.go(parseInt(String(v), 10) || 0, "api"); }
  getValue(): number { return this.index(); }
  forward(): void {
    const cur = this.index();
    if (cur + 1 < this.pages().length) { this.go(cur + 1, "api"); }
  }
  back(): void {
    const cur = this.index();
    if (cur > 0) { this.go(cur - 1, "api"); }
  }
  startSlideShow(): void {
    const el = this.element;
    if (this.#timer) { return; }
    this.#timer = setInterval(() => {
      if (this.#paused || this.disabled()) { return; }
      const cnt = this.pages().length, cur = this.index();
      this.go(cur + 1 >= cnt ? 0 : cur + 1, "auto");
    }, num(el, "data-slide-duration", 3000));
  }
  stopSlideShow(): void {
    if (this.#timer !== null) { clearInterval(this.#timer); }
    this.#timer = null;
  }
  refresh(): void {
    const cnt = this.pages().length, cur = Math.min(this.index(), Math.max(0, cnt - 1));
    setValue(this.element, String(cur));
    this.place(cur);
    this.mark(cur);
  }

  private wrapper(): HTMLElement | null {
    return this.element.querySelector<HTMLElement>(":scope > ." + SV + "-wrapper");
  }
  private pages(): HTMLElement[] {
    const w = this.wrapper();
    return w ? Array.from(w.querySelectorAll<HTMLElement>(":scope > ." + SV + "-page")) : [];
  }
  private index(): number { return parseInt(this.element.getAttribute("data-ah-value") || "", 10) || 0; }
  private disabled(): boolean { return this.element.classList.contains(SV + "-disabled"); }

  private place(idx: number): void {
    const w = this.wrapper();
    if (w) { w.style.marginLeft = idx ? (-idx * 100) + "%" : ""; }
  }

  private mark(idx: number): void {
    this.pages().forEach((pg, i) => {
      if (i === idx) {
        pg.removeAttribute("aria-hidden");
        pg.removeAttribute("inert");
      } else {
        pg.setAttribute("aria-hidden", "true");
        pg.setAttribute("inert", "");
      }
    });
    this.element.querySelectorAll(":scope > ." + SV + "-buttons > ." + SV + "-button").forEach((b, i) => {
      const on = i === idx;
      b.classList.toggle(SV + "-button-active", on);
      if (on) { b.setAttribute("aria-current", "true"); } else { b.removeAttribute("aria-current"); }
    });
  }

  // Turn the transition on for one move and off again when it ends.
  private animate(): void {
    const el = this.element;
    el.classList.add(SV + "-animating");
    if (this.#anim !== null) { clearTimeout(this.#anim); }
    this.#anim = setTimeout(() => {
      this.#anim = null;
      el.classList.remove(SV + "-animating");
    }, num(el, "data-duration", 300) + 50);
  }

  private go(target: number, how: How): void {
    const cnt = this.pages().length;
    if (!cnt) { return; }
    const idx = Math.max(0, Math.min(cnt - 1, target));
    const old = this.index();
    this.animate();
    this.place(idx);
    if (idx === old) { return; }
    this.mark(idx);
    setValue(this.element, String(idx));
    this.fire<ScrollviewPageChanged>("ah:page-changed", { page: idx, old });
    if (how === "user") { this.fire("change"); }
  }
}

AH.register("scrollview", ScrollviewController);
