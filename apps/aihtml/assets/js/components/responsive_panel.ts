/* Behaviour of the responsive_panel component (designs/04-components.md),
 * ported from sigil's responsive-panel. The only listener on document is
 * the click-outside one, removed on teardown. Events on the root (no
 * detail): ah:collapse / ah:expand (folding at the breakpoint), ah:open /
 * ah:close (the folded content); ah:load fires once on the content.
 */
import AH from "../core.ts";
import type { FloatHandle } from "../core.ts";
import { num } from "./_lib_scroll.ts";
import { animate, fade, hide, slide, stop } from "./_lib_layout.ts";

// ------------------------------------------------------------------
// Responsive panel (sigil layout/responsive_panel)
// ------------------------------------------------------------------
//
// Folded when the parent is at most data-breakpoint px wide: the content
// then floats below the toggle (AH.float) while open.

const RP = "ah-responsive-panel";

function clearStyles(c: HTMLElement | null): void {
  if (!c) { return; }
  stop(c);
  c.style.display = c.style.opacity = c.style.width = "";
}

function parentWidth(el: Element): number {
  const p = el.parentElement;
  if (!p) { return 0; }
  const cs = getComputedStyle(p);
  return p.clientWidth - (parseFloat(cs.paddingLeft) || 0) - (parseFloat(cs.paddingRight) || 0);
}

class ResponsivePanelController extends AH.Controller {
  #collapsed = false;
  #open = false;
  #loaded = false;
  #float: FloatHandle | null = null;
  #ro: ResizeObserver | null = null;
  #ext: Element[] = [];
  #live = false;

  override setup(): void {
    const el = this.element;
    this.#collapsed = this.#open = this.#loaded = false;
    this.#float = null;
    this.#ro = null;
    this.#ext = [];
    this.#live = true;
    const t = this.toggleEl();
    if (t) {
      this.listen(t, "click", () => {
        if (!this.disabled()) { this.flip(); }
      });
      this.listen(t, "keydown", (e) => {
        if (e.key === "Enter" || e.key === " ") {
          e.preventDefault();
          if (!this.disabled()) { this.flip(); }
        }
      });
    }
    this.listen(el, "keydown", (e) => {
      if (e.key === "Escape" && this.#open) {
        e.stopPropagation();
        this.close();
        const tg = this.toggleEl();
        if (tg) { tg.focus(); }
      }
    });
    const sel = el.getAttribute("data-toggle-button");
    if (sel) {
      this.#ext = Array.from(document.querySelectorAll(sel));
      this.#ext.forEach((b) => {
        this.listen(b, "click", () => {
          if (!this.disabled()) { this.flip(); }
        });
      });
    }
    this.listen(document, "click", (e) => {
      const target = e.target as Node | null;
      if (this.#open && el.getAttribute("data-auto-close") !== "false" &&
          !el.contains(target) &&
          !this.#ext.some((b) => b.contains(target))) {
        this.close();
      }
    });
    const check = (): void => { this.check(); };
    if (window.ResizeObserver && el.parentNode instanceof Element) {
      this.#ro = new ResizeObserver(check);
      this.#ro.observe(el.parentNode);
    }
    this.listen(window, "resize", check);
    check();
    if (!this.#collapsed) { this.load(); }
  }

  override teardown(): void {
    if (this.#float) { this.#float.stop(); }
    if (this.#ro) { this.#ro.disconnect(); }
    const c = this.content();
    if (c) { stop(c); }
    this.#float = null;
    this.#ro = null;
    this.#live = false;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void {
    const el = this.element;
    if (!this.#collapsed || this.#open || this.disabled()) { return; }
    const c = this.content();
    const t = this.toggleEl();
    if (!c || !t) { return; }
    const anim = el.getAttribute("data-animation") || "fade";
    const speed = num(el, "data-show-duration", 200);
    const cw = el.getAttribute("data-collapse-width");
    clearStyles(c);
    if (cw) { c.style.width = /^\d+(\.\d+)?$/.test(cw) ? cw + "px" : cw; }
    this.#open = true;
    el.classList.add(RP + "-open");
    t.setAttribute("aria-expanded", "true");
    this.#float = AH.float(c, t, { placement: "bottom", align: "start", offset: 4 });
    const shown = (): void => {
      if (this.#float) { this.#float.update(); }
      this.fire("ah:open");
    };
    if (anim === "fade") {
      c.style.opacity = "0";
      animate(c, [{ opacity: 0 }, { opacity: 1 }], speed, () => {
        c.style.opacity = "1";
        shown();
      });
    } else if (anim === "slide") {
      hide(c);
      slide(c, true, speed, shown);
    } else {
      shown();
    }
    this.load();
  }

  close(instant?: boolean): void {
    const el = this.element;
    if (!this.#open) { return; }
    const c = this.content();
    const t = this.toggleEl();
    const anim = instant === true ? "none" : (el.getAttribute("data-animation") || "fade");
    const speed = num(el, "data-hide-duration", 200);
    this.#open = false;
    if (t) { t.setAttribute("aria-expanded", "false"); }
    if (c && c.contains(document.activeElement) && t) { t.focus(); }
    const hidden = (): void => {
      el.classList.remove(RP + "-open");
      if (this.#float) { this.#float.stop(); this.#float = null; }
      if (c) { c.style.display = c.style.opacity = ""; }
      if (instant !== true) { this.fire("ah:close"); }
    };
    if (c) { stop(c); }
    if (c && anim === "fade") { fade(c, false, speed, hidden); }
    else if (c && anim === "slide") { slide(c, false, speed, hidden); }
    else { hidden(); }
  }

  toggle(): void { if (this.#collapsed) { this.flip(); } }
  refresh(): void { this.check(); }
  isCollapsed(): boolean { return this.#collapsed; }
  isOpen(): boolean { return this.#open; }

  private toggleEl(): HTMLElement | null {
    return this.element.querySelector<HTMLElement>(":scope > ." + RP + "-toggle");
  }
  private content(): HTMLElement | null {
    return this.element.querySelector<HTMLElement>(":scope > ." + RP + "-content");
  }
  private disabled(): boolean { return this.element.classList.contains(RP + "-disabled"); }

  private load(): void {
    const c = this.content();
    if (this.#loaded) { return; }
    this.#loaded = true;
    if (c && /(^|\s)ah:load:/.test(c.getAttribute("data-ah-on") || "")) { this.fire("ah:load", undefined, c); }
  }

  private flip(): void {
    if (this.#open) { this.close(); } else { this.open(); }
  }

  private check(): void {
    const el = this.element;
    if (!this.#live) { return; }
    const bp = num(el, "data-breakpoint", 1000);
    const pw = parentWidth(el);
    if (!this.#collapsed && pw <= bp) {
      if (this.#open) { this.close(true); }
      this.#collapsed = true;
      el.classList.add(RP + "-collapsed");
      this.fire("ah:collapse");
    } else if (this.#collapsed && pw > bp) {
      this.close(true);
      this.#collapsed = false;
      el.classList.remove(RP + "-collapsed", RP + "-open");
      clearStyles(this.content());
      this.fire("ah:expand");
      this.load();
    }
  }
}

AH.register("responsive-panel", ResponsivePanelController);
