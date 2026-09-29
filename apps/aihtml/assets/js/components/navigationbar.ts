/* Behaviour of the navigation bar (designs/04-components.md), ported from
 * sigil's layout/navigationbar. The value (the expanded indexes "0,2") is
 * kept in data-ah-value on the root, mirrored into a hidden input, and
 * "change" fires when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) do not fire "change". ah:expand /
 * ah:collapse fire on the root when a section's animation ends, detail
 * NavigationbarToggle {index}. */
import AH from "../core.ts";
import { fade, hide, setValue, show, slide, stop } from "./_lib_layout.ts";

/** Detail of ah:expand / ah:collapse. */
export interface NavigationbarToggle { index: number; }

function px(cs: CSSStyleDeclaration, prop: "borderTopWidth" | "borderBottomWidth"): number {
  return parseFloat(cs[prop]) || 0;
}

// ------------------------------------------------------------------
// NavigationBar: collapsible sections
// ------------------------------------------------------------------

class NavigationbarController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    const mode = el.getAttribute("data-toggle-mode") || "click";
    const own = (h: Element): boolean => h.closest(".ah-navigationbar") === el;
    if (mode !== "none") {
      this.delegate<Event, HTMLElement>(mode, ".ah-navigationbar-header", (_e, h) => {
        if (own(h)) { this.userToggle(this.headers().indexOf(h)); }
      });
    }
    this.delegate("keydown", ".ah-navigationbar-header", (e, h) => {
      if (!own(h)) { return; }
      const hs = this.headers().filter((x) => x.getAttribute("tabindex") === "0");
      const i = hs.indexOf(h);
      let t: HTMLElement | undefined;
      switch (e.key) {
        case "Enter": case " ":
          e.preventDefault();
          if (mode !== "none") { this.userToggle(this.headers().indexOf(h)); }
          return;
        case "ArrowDown": t = hs[(i + 1) % hs.length]; break;
        case "ArrowUp": t = hs[(i - 1 + hs.length) % hs.length]; break;
        case "Home": t = hs[0]; break;
        case "End": t = hs[hs.length - 1]; break;
        default: return;
      }
      e.preventDefault();
      if (t) { t.focus(); }
    });
    this.fit();
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  expand(i: number | string): void { this.expandAt(Number(i)); }
  collapse(i: number | string): void { this.collapseAt(Number(i)); }
  toggle(i: number | string): void {
    const n = Number(i);
    if (this.expanded().indexOf(n) >= 0) { this.collapseAt(n); } else { this.expandAt(n); }
  }
  setValue(v: unknown): void {
    const raw: unknown[] = Array.isArray(v) ? v : String(v == null ? "" : v).split(",");
    const want = raw
      .filter((s) => s !== "").map(Number);
    this.expanded().forEach((i) => {
      if (want.indexOf(i) < 0) { this.collapseAt(i); }
    });
    want.forEach((i) => {
      if (this.expanded().indexOf(i) < 0 && this.valid(i)) {
        // setValue may open several sections whatever the mode
        this.store(this.expanded().concat([i]));
        this.animate(i, true);
      }
    });
  }
  getValue(): number[] { return this.expanded(); }
  enable(i: number | string): void {
    const h = this.header(Number(i));
    if (h) {
      h.classList.remove("ah-navigationbar-disabled");
      h.removeAttribute("aria-disabled");
      h.setAttribute("tabindex", "0");
    }
  }
  disable(i: number | string): void {
    const h = this.header(Number(i));
    if (h) {
      h.classList.add("ah-navigationbar-disabled");
      h.setAttribute("aria-disabled", "true");
      h.setAttribute("tabindex", "-1");
    }
  }

  private items(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(":scope > .ah-navigationbar-item"));
  }

  private headers(): HTMLElement[] {
    return Array.from(this.element.querySelectorAll<HTMLElement>(
      ":scope > .ah-navigationbar-item > .ah-navigationbar-header"));
  }

  private header(i: number): HTMLElement | null {
    const it = this.items()[i];
    return it ? it.querySelector<HTMLElement>(":scope > .ah-navigationbar-header") : null;
  }

  private expanded(): number[] {
    return (this.element.getAttribute("data-ah-value") || "").split(",").filter((s) => s !== "").map(Number);
  }

  private store(list: number[]): void {
    const uniq = list.filter((v, i) => list.indexOf(v) === i);
    uniq.sort((a, b) => a - b);
    setValue(this.element, uniq.join(","));
  }

  private mode(): string {
    return this.element.getAttribute("data-expand-mode") || "single_fit_height";
  }

  // single_fit_height with a fixed height: the open body fills what the
  // headers leave (sigil's sync-content-height!).
  private fit(): void {
    const el = this.element;
    if (!el.hasAttribute("data-fit")) { return; }
    let used = 0;
    this.items().forEach((it) => {
      const h = it.querySelector<HTMLElement>(":scope > .ah-navigationbar-header");
      const cs = getComputedStyle(it);
      // header's outer height (border box) + the item's borders
      used += (h ? h.offsetHeight : 0) + px(cs, "borderTopWidth") + px(cs, "borderBottomWidth");
    });
    // the root's inner height (padding box)
    const h = Math.max(0, el.clientHeight - used);
    el.querySelectorAll<HTMLElement>(":scope > .ah-navigationbar-item > .ah-navigationbar-body").forEach((b) => {
      if (b.style.display !== "none") { b.style.height = h + "px"; }
    });
  }

  private animate(i: number, open: boolean): void {
    const el = this.element;
    const h = this.header(i);
    const body = this.items()[i].querySelector<HTMLElement>(":scope > .ah-navigationbar-body");
    const anim = el.getAttribute("data-animation") || "slide";
    let ms = parseInt(el.getAttribute(open ? "data-expand-duration" : "data-collapse-duration") || "", 10);
    if (isNaN(ms)) { ms = 250; }
    if (h) {
      h.classList.toggle("ah-navigationbar-header-expanded", open);
      h.setAttribute("aria-expanded", String(open));
      h.querySelectorAll(":scope > .ah-navigationbar-arrow").forEach((a) => {
        a.classList.toggle("ah-navigationbar-arrow-up", open);
      });
    }
    const done = (): void => {
      if (body) {
        if (open) {
          show(body);
          this.fit();
        } else {
          hide(body);
        }
      }
      this.fire<NavigationbarToggle>(open ? "ah:expand" : "ah:collapse", { index: i });
    };
    if (!body) { done(); return; }
    stop(body);
    if (anim === "slide") {
      slide(body, open, ms, done);
    } else if (anim === "fade") {
      fade(body, open, ms, done);
    } else {
      done();
    }
  }

  private valid(i: number): boolean {
    return typeof i === "number" && i >= 0 && i < this.items().length;
  }

  private disabledAt(i: number): boolean {
    const h = this.header(i);
    return !!h && h.classList.contains("ah-navigationbar-disabled");
  }

  private collapseAt(i: number): boolean {
    const cur = this.expanded();
    if (!this.valid(i) || cur.indexOf(i) < 0) { return false; }
    this.store(cur.filter((x) => x !== i));
    this.animate(i, false);
    return true;
  }

  private expandAt(i: number): boolean {
    const cur = this.expanded();
    if (!this.valid(i) || this.disabledAt(i) || cur.indexOf(i) >= 0) { return false; }
    if (this.mode() !== "multiple") {
      cur.forEach((x) => { this.collapseAt(x); });
    }
    this.store(this.expanded().concat([i]));
    this.animate(i, true);
    return true;
  }

  // A user toggle, as sigil's compute-proposed-indexes: single modes never
  // close the open section, none never changes.
  private userToggle(i: number): void {
    const mode = this.mode();
    if (this.element.classList.contains("ah-navigationbar-disabled") || this.disabledAt(i) ||
        mode === "none") {
      return;
    }
    let changed: boolean;
    if (this.expanded().indexOf(i) >= 0) {
      changed = (mode === "single" || mode === "single_fit_height") ? false : this.collapseAt(i);
    } else {
      changed = this.expandAt(i);
    }
    if (changed) { this.fire("change"); }
  }
}

AH.register("navigationbar", NavigationbarController);
