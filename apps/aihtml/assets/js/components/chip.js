/* chip behaviour (designs/04-components.md): remove button, keyboard.
   "ah:remove" (cancelable, detail {value}) then "change" on the root
   before a removable chip goes away. */
import AH from "../core.js";
import "./_lib_display.js";

const L = AH.lib.display;

function sib(node, dir) {
  const n = node && node[dir];
  return n && n.matches("[tabindex]") ? n : null;
}

AH.register("chip", class extends AH.Controller {
  setup() {
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
        L.keyClick(e, el);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  remove() { this.removeChip(); }

  removeChip() {
    const el = this.element;
    if (!this.fire("ah:remove", { value: el.getAttribute("data-ah-value") })) { return false; }
    // value contract: the root's change carries data-ah-value
    this.fire("change");
    L.drop(el);
    return true;
  }
});
