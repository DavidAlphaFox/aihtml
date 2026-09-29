/* The popover behaviour (designs/04-components.md), ported from sigil's
 * overlay/popover: a bubble anchored to the element that opened it or to
 * its data-ah-anchor selector. Shared machinery: _lib_overlay.ts.
 * Events: ah:opening (cancelable), ah:open, ah:close (detail
 * OverlayClose). */
import AH from "../core.ts";
import { ArrowFloat, fades, flag, matching, overlays, placement } from "./_lib_overlay.ts";
import type { OpenOptions, OverlayClose } from "./_lib_overlay.ts";

export type { OverlayClose } from "./_lib_overlay.ts";

const POP_POSITIONS = "ah-popover-top ah-popover-bottom ah-popover-left ah-popover-right";

class PopoverController extends AH.Controller {
  #anchor: Element | null = null;
  #float: ArrowFloat | null = null;
  #backdrop: HTMLElement | null = null;

  override setup(): void {
    const el = this.element;
    this.#anchor = null;
    this.#float = null;
    this.#backdrop = null;
    const anchorSel = el.getAttribute("data-ah-anchor");
    if (anchorSel) {
      // sigil's `selector' prop: the anchor toggles the popover
      this.listen(document, "click", (e) => {
        let hits: Element[];
        try { hits = matching(e.target, anchorSel); } catch { return; }
        hits.forEach((a) => {
          if (a.matches("[data-ah-open],[data-ah-toggle]")) { return; }
          e.preventDefault();
          AH.invoke(el, "toggle", { invoker: a });
        });
      });
    }
    this.listen(document, "click", (e) => {
      if (!this.isOpen() || !flag(el, "auto-close", true) || flag(el, "modal", false)) { return; }
      const anchor = this.#anchor;
      const t = e.target;
      if (t instanceof Node && (el.contains(t) || (anchor && anchor.contains(t)))) { return; }
      this.close();
    });
  }

  override teardown(): void {
    this.#unfloat();
    if (this.#backdrop) { this.#backdrop.remove(); }
    overlays.pull(this.element, false);
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(opts?: OpenOptions | null): void {
    const el = this.element;
    if (this.isOpen()) { return; }
    const sel = el.getAttribute("data-ah-anchor");
    const anchor = (opts && opts.invoker) || (sel ? document.querySelector(sel) : null);
    if (!anchor) {
      console.error("aihtml: popover has no anchor", el);
      return;
    }
    if (!this.fire("ah:opening")) { return; }
    this.#anchor = anchor;
    const zi = overlays.nextZ();
    fades.stop(el, true);
    this.#unfloat();
    Object.assign(el.style, { zIndex: String(zi), display: "block", visibility: "hidden", opacity: "0" });
    this.#float = new ArrowFloat(el, anchor, placement(el.getAttribute("data-ah-position"), "bottom"),
                                 8, "ah-popover-", POP_POSITIONS);
    if (flag(el, "modal", false) && el.parentNode) {
      const bd = document.createElement("div");
      bd.className = "ah-popover-modal-backdrop";
      bd.style.zIndex = String(zi - 1);
      el.parentNode.insertBefore(bd, el);
      this.#backdrop = bd;
    }
    el.style.visibility = "visible";
    fades.fade(el, 1, 200);
    el.setAttribute("data-state", "open");
    anchor.setAttribute("aria-expanded", "true");
    overlays.push({ el: el, trap: null, esc: () => { this.close(); return true; } });
    this.fire("ah:open");
  }

  close(result?: unknown): void {
    const el = this.element;
    if (!this.isOpen()) { return; }
    const anchor = this.#anchor;
    el.setAttribute("data-state", "closed");
    if (anchor) { anchor.setAttribute("aria-expanded", "false"); }
    if (this.#backdrop) {
      this.#backdrop.remove();
      this.#backdrop = null;
    }
    const focusInside = el !== document.activeElement && el.contains(document.activeElement);
    overlays.pull(el, false);
    if (focusInside && (anchor instanceof HTMLElement || anchor instanceof SVGElement)) { anchor.focus(); }
    fades.fadeOut(el, 200, () => { this.#unfloat(); });
    this.fire<OverlayClose>("ah:close", { result: result || null });
  }

  toggle(opts?: OpenOptions | null): void {
    if (this.isOpen()) { this.close(); } else { this.open(opts); }
  }

  isOpen(): boolean { return this.element.getAttribute("data-state") === "open"; }

  #unfloat(): void {
    if (this.#float) {
      this.#float.stop();
      this.#float = null;
    }
  }
}

AH.register("popover", PopoverController);
