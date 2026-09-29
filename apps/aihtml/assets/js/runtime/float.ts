// Floating popups: AH.float(popup, anchor, opts) pins a popup next to its
// anchor with position: fixed, so no ancestor with overflow: hidden (a
// card, a panel, a scroll area) can clip it, and keeps it there while the
// page scrolls or resizes. It flips to the other side when the preferred
// one lacks room and stays inside the viewport. Call stop() when the popup
// closes.
import { one } from "./dom.ts";
import type { Targets } from "./dom.ts";

export type Placement = "bottom" | "top" | "right" | "left";

export interface FloatOptions {
  /** default "bottom" */
  placement?: Placement;
  /** default "start" */
  align?: "start" | "end" | "center";
  /** gap in px, default 4 */
  offset?: number;
  /** at least the anchor's width (for lists), default false */
  matchWidth?: boolean;
}

export interface FloatHandle {
  update(): void;
  stop(): void;
}

const OTHER: Record<Placement, Placement> = { bottom: "top", top: "bottom", right: "left", left: "right" };

export class FloatingPopup implements FloatHandle {
  static #active: FloatingPopup[] = [];
  static readonly #updateAll = (): void => { FloatingPopup.#active.forEach((h) => { h.update(); }); };

  readonly popup: HTMLElement | SVGElement;
  readonly anchor: Element;
  readonly #opts: Required<FloatOptions>;

  constructor(popup: Targets, anchor: Targets, opts?: FloatOptions) {
    const p = one(popup), a = one(anchor);
    if (!(p instanceof HTMLElement || p instanceof SVGElement) || !(a instanceof Element)) {
      throw new Error("aihtml: float needs a popup and an anchor element");
    }
    this.popup = p;
    this.anchor = a;
    this.#opts = { placement: "bottom", align: "start", offset: 4, matchWidth: false, ...opts };
    if (!FloatingPopup.#active.length) {
      window.addEventListener("resize", FloatingPopup.#updateAll);
      window.addEventListener("scroll", FloatingPopup.#updateAll, true);
    }
    FloatingPopup.#active.push(this);
    this.update();
  }

  stop(): void {
    FloatingPopup.#active = FloatingPopup.#active.filter((h) => h !== this);
    const s = this.popup.style;
    s.position = s.top = s.left = s.right = s.bottom = s.minWidth = "";
    this.popup.removeAttribute("data-ah-placement");
    if (!FloatingPopup.#active.length) {
      window.removeEventListener("resize", FloatingPopup.#updateAll);
      window.removeEventListener("scroll", FloatingPopup.#updateAll, true);
    }
  }

  update(): void {
    const { popup, anchor } = this, opts = this.#opts;
    if (!document.contains(anchor)) { return; }
    const a = anchor.getBoundingClientRect();
    popup.style.position = "fixed";
    popup.style.right = popup.style.bottom = "auto";
    if (opts.matchWidth) { popup.style.minWidth = a.width + "px"; }
    popup.style.top = "0px";
    popup.style.left = "0px";
    const p = popup.getBoundingClientRect();
    const vw = document.documentElement.clientWidth, vh = document.documentElement.clientHeight;
    const gap = opts.offset;
    let side = opts.placement, top: number, left: number;
    const room: Record<Placement, number> = { bottom: vh - a.bottom, top: a.top, right: vw - a.right, left: a.left };
    const need = (side === "bottom" || side === "top") ? p.height + gap : p.width + gap;
    const other = OTHER[side];
    if (room[side] < need && room[other] > room[side]) { side = other; }
    if (side === "bottom" || side === "top") {
      top = side === "bottom" ? a.bottom + gap : a.top - gap - p.height;
      left = opts.align === "end" ? a.right - p.width
           : opts.align === "center" ? a.left + (a.width - p.width) / 2 : a.left;
    } else {
      left = side === "right" ? a.right + gap : a.left - gap - p.width;
      top = opts.align === "end" ? a.bottom - p.height
          : opts.align === "center" ? a.top + (a.height - p.height) / 2 : a.top;
    }
    left = Math.max(4, Math.min(left, vw - p.width - 4));
    top = Math.max(4, Math.min(top, vh - p.height - 4));
    popup.style.top = Math.round(top) + "px";
    popup.style.left = Math.round(left) + "px";
    popup.setAttribute("data-ah-placement", side);
  }
}
