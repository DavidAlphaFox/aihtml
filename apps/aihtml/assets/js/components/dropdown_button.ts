/* dropdown-button: a menu popup (outside click / Escape close, arrow
 * keys); choosing an item sets data-ah-value and fires change (detail
 * ButtonChange: the item's value); "ah:open" / "ah:close" (no detail)
 * (designs/04-components.md). The menu code is ButtonMenu in
 * _lib_button.ts. */
import AH from "../core.ts";
import { ButtonMenu, setValue } from "./_lib_button.ts";
import type { MenuConfig } from "./_lib_button.ts";

function popupOf(el: Element): HTMLElement | null {
  return el.querySelector<HTMLElement>(":scope > .ah-dropdown-btn-popup");
}

const DROPDOWN: MenuConfig = {
  trigger: ".ah-dropdown-btn-wrapper",
  menu: ".ah-dropdown-btn-popup",
  item: ".ah-dropdown-btn-item",
  disabled: (el) => el.classList.contains("ah-dropdown-btn-disabled"),
  isOpen: (el) => el.classList.contains("ah-dropdown-btn-opened"),
  show: (el, on, m) => {
    const popup = popupOf(el);
    el.classList.toggle("ah-dropdown-btn-opened", on);
    if (!popup) { return; }
    if (on) {
      popup.removeAttribute("hidden");
      m.float(popup, { placement: "bottom", align: "start", matchWidth: true });
    } else {
      popup.setAttribute("hidden", "");
      m.unfloat(0);
    }
  },
  select: (el, item) => {
    el.querySelectorAll(".ah-dropdown-btn-item").forEach((i) => { i.classList.remove("selected"); });
    item.classList.add("selected");
  }
};

class DropdownButtonController extends AH.Controller {
  #menu: ButtonMenu | null = null;

  override setup(): void {
    const el = this.element;
    const menu = this.#menu = new ButtonMenu(this, DROPDOWN);
    this.listen(el, "mouseenter", () => {
      if (DROPDOWN.disabled(el)) { return; }
      el.classList.add("ah-dropdown-btn-hover");
      if (el.classList.contains("ah-dropdown-btn-auto-open")) { menu.open(false); }
    });
    this.listen(el, "mouseleave", () => {
      el.classList.remove("ah-dropdown-btn-hover");
      if (el.classList.contains("ah-dropdown-btn-auto-open")) { menu.close(false); }
    });
    const trigger = el.querySelector(":scope > .ah-dropdown-btn-wrapper");
    if (trigger) {
      this.listen(trigger, "focus", () => {
        // sigil shows the focus ring on focus; keep it to keyboard focus
        let visible = true;
        try { visible = trigger.matches(":focus-visible"); } catch { /* old browser */ }
        if (visible) { el.classList.add("ah-dropdown-btn-focused"); }
      });
      this.listen(trigger, "blur", () => { el.classList.remove("ah-dropdown-btn-focused"); });
    }
  }

  override teardown(): void {
    if (this.#menu) { this.#menu.destroy(); }
    this.#menu = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void { if (this.#menu) { this.#menu.open(false); } }
  close(): void { if (this.#menu) { this.#menu.close(false); } }
  toggle(): void {
    if (DROPDOWN.isOpen(this.element)) { this.close(); } else { this.open(); }
  }
  setValue(v: unknown): void {
    const el = this.element;
    el.querySelectorAll(".ah-dropdown-btn-item").forEach((i) => {
      i.classList.toggle("selected", i.getAttribute("data-value") === String(v));
    });
    setValue(el, v, false);
  }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
}

AH.register("dropdown-button", DropdownButtonController);
