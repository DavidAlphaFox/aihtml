/* split-button: main action + menu; arrow and menu clicks do not reach
 * the root, so on(click) there is the main action. Choosing an item sets
 * data-ah-value and fires change (detail ButtonChange: the item's value);
 * "ah:open" / "ah:close" (no detail) (designs/04-components.md). The menu
 * code is ButtonMenu in _lib_button.ts. */
import AH from "../core.ts";
import { ButtonMenu, setValue } from "./_lib_button.ts";
import type { MenuConfig } from "./_lib_button.ts";

const SPLIT: MenuConfig = {
  trigger: ".ah-split-button__arrow",
  menu: ".ah-split-button__menu",
  item: ".ah-split-button__item",
  disabled: (el) => el.getAttribute("data-disabled") === "true",
  isOpen: (el) => el.getAttribute("data-open") === "true",
  show: (el, on, m) => {
    el.setAttribute("data-open", on ? "true" : "false");
    const menu = el.querySelector<HTMLElement>(":scope > .ah-split-button__menu");
    if (on) {
      if (menu) {
        m.float(menu, { placement: "bottom",
                        align: el.getAttribute("data-menu-align") === "start" ? "start" : "end" });
      }
    } else {
      m.unfloat(150);
    }
  },
  select: () => {}
};

class SplitButtonController extends AH.Controller {
  #menu: ButtonMenu | null = null;

  override setup(): void {
    this.#menu = new ButtonMenu(this, SPLIT);
    // Only the main half's clicks reach the root (and its on(click)).
    this.element.querySelectorAll(".ah-split-button__arrow, .ah-split-button__menu").forEach((n) => {
      this.listen(n, "click", (e) => { e.stopPropagation(); });
    });
  }

  override teardown(): void {
    if (this.#menu) { this.#menu.destroy(); }
    this.#menu = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  open(): void { if (this.#menu) { this.#menu.open(false); } }
  close(): void { if (this.#menu) { this.#menu.close(false); } }
  setValue(v: unknown): void { setValue(this.element, v, false); }
  getValue(): string | null { return this.element.getAttribute("data-ah-value"); }
}

AH.register("split-button", SplitButtonController);
