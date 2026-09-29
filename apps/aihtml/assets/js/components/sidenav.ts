/* Behaviour of the sidenav component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.ts.
   Value contract: data-ah-value (the active link's data-route), the
   hidden input and "change" on the root. ah:collapse fires on the root
   when it collapses or expands, detail SidenavCollapse. */
import AH from "../core.ts";
import { byKey, setValue, visible } from "./_lib_nav.ts";

/** Detail of ah:collapse: whether the sidebar is now collapsed. */
export interface SidenavCollapse { collapsed: boolean; }

// ------------------------------------------------------------------
// sidenav (sigil sidenav.cljs + nav-tree)
// ------------------------------------------------------------------

function summaries(details: Element): NodeListOf<Element> {
  return details.querySelectorAll(":scope > summary");
}

class SidenavController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate("click", "a.ah-nav-tree__item", (e, a) => {
      if (a.getAttribute("aria-disabled") === "true") { e.preventDefault(); return; }
      const key = a.getAttribute("data-route");
      this.#mark(key);
      if (a.getAttribute("href") === "#") {
        e.preventDefault();
        if (el.getAttribute("data-ah-value") !== key) { setValue(el, key, "change"); }
      }
    });
    // a collapsed sidebar expands when a group is opened
    this.delegate("click", "summary.ah-nav-tree__item", (e, s) => {
      if (el.classList.contains("ah-sidenav-collapsed")) {
        e.preventDefault();
        this.#collapse(false);
        if (s.parentElement instanceof HTMLDetailsElement) { s.parentElement.open = true; }
        s.classList.add("ah-is-open");
      }
    });
    // toggle does not bubble: listen in the capture phase
    this.listen(el, "toggle", (e) => {
      const d = e.target;
      if (d instanceof HTMLDetailsElement) {
        summaries(d).forEach((s) => { s.classList.toggle("ah-is-open", d.open); });
      }
    }, { capture: true });
    this.delegate("click", ".ah-sidenav__toggle", () => {
      this.#collapse(!el.classList.contains("ah-sidenav-collapsed"));
    });
    // arrow keys move between the visible entries
    this.delegate("keydown", ".ah-nav-tree__item", (e, cur) => {
      if (!/^(ArrowDown|ArrowUp|Home|End)$/.test(e.key)) { return; }
      const items = Array.from(el.querySelectorAll<HTMLElement>(".ah-nav-tree__item")).filter(visible);
      const i = items.indexOf(cur);
      const to = e.key === "Home" ? 0 : e.key === "End" ? items.length - 1
        : Math.max(0, Math.min(items.length - 1, i + (e.key === "ArrowDown" ? 1 : -1)));
      e.preventDefault();
      if (items[to]) { items[to].focus(); }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(key: string | number | null): void { this.#mark(key); setValue(this.element, key); }
  collapse(): void { this.#collapse(true); }
  expand(): void { this.#collapse(false); }
  toggle(): void { this.#collapse(!this.element.classList.contains("ah-sidenav-collapsed")); }

  #mark(key: string | number | null): void {
    const el = this.element;
    el.querySelectorAll(".ah-nav-tree__item.ah-is-active").forEach((n) => {
      n.classList.remove("ah-is-active");
      n.removeAttribute("aria-current");
    });
    byKey(el, "a.ah-nav-tree__item", "data-route", key).forEach((a) => {
      a.classList.add("ah-is-active");
      a.setAttribute("aria-current", "page");
      for (let d = a.parentElement; d && d !== el; d = d.parentElement) {
        if (d instanceof HTMLDetailsElement && d.matches("details.ah-nav-tree__node")) {
          d.open = true;
          summaries(d).forEach((s) => { s.classList.add("ah-is-open"); });
        }
      }
    });
  }

  #collapse(collapsed: boolean): void {
    const el = this.element;
    el.classList.toggle("ah-sidenav-collapsed", collapsed);
    el.querySelectorAll(".ah-sidenav__toggle").forEach((t) => {
      t.setAttribute("aria-expanded", String(!collapsed));
    });
    this.fire<SidenavCollapse>("ah:collapse", { collapsed: collapsed });
  }
}

AH.register("sidenav", SidenavController);
