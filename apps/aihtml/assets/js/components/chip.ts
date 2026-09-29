/* chip behaviour (designs/04-components.md): remove button, keyboard.
   "ah:remove" (cancelable, detail ChipRemove {value}) then "change" on
   the root before a removable chip goes away. */
import AH from "../core.ts";
import { drop, keyClick } from "./_lib_display.ts";

/** Detail of ah:remove: the chip's data-ah-value. */
export interface ChipRemove { value: string | null; }

function sib(node: Element, dir: "nextElementSibling" | "previousElementSibling"): HTMLElement | null {
  const n = node[dir];
  return n instanceof HTMLElement && n.matches("[tabindex]") ? n : null;
}

class ChipController extends AH.Controller {
  override setup(): void {
    const el = this.element;
    this.delegate("click", ".ah-chip__delete", (e) => {
      e.stopPropagation();
      if (el.getAttribute("data-disabled") !== "true") { this.removeChip(); }
    });
    this.listen(el, "keydown", (e) => {
      if (e.target !== el || el.getAttribute("data-disabled") === "true") { return; }
      if ((e.key === "Backspace" || e.key === "Delete") && el.querySelector(".ah-chip__delete")) {
        e.preventDefault();
        const next = sib(el, "nextElementSibling") || sib(el, "previousElementSibling");
        if (this.removeChip() && next) { next.focus(); }
      } else if (el.getAttribute("data-clickable") === "true") {
        keyClick(e, el);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  remove(): void { this.removeChip(); }

  private removeChip(): boolean {
    const el = this.element;
    if (!this.fire<ChipRemove>("ah:remove", { value: el.getAttribute("data-ah-value") })) { return false; }
    // value contract: the root's change carries data-ah-value
    this.fire("change");
    drop(el);
    return true;
  }
}

AH.register("chip", ChipController);
