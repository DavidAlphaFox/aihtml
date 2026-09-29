/* segmented-control: single selection, arrow keys move and select; the
 * root keeps data-ah-value and the hidden input in step and fires change
 * (detail: the new value) (designs/04-components.md). */
import AH from "../core.js";
import "./_lib_button.js";

var L = AH.lib.button;

AH.register("segmented-control", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.delegate("click", ".ah-segmented-control__item", (e, it) => {
      if (it.disabled || el.getAttribute("data-disabled") === "true"
          || it.getAttribute("data-disabled") === "true") { return; }
      this.set(it.getAttribute("data-value"), true);
    });
    this.delegate("keydown", ".ah-segmented-control__item", (e, it) => {
      var items = this.items().filter((i) => !i.disabled);
      var i = L.step(e.key, items.indexOf(it), items.length);
      if (i < 0) { return; }
      e.preventDefault();
      var to = items[i];
      to.focus();
      this.set(to.getAttribute("data-value"), true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) { this.set(v, false); }
  getValue() { return this.element.getAttribute("data-ah-value"); }

  items() {
    return Array.from(this.element.children)
      .filter((i) => i.classList.contains("ah-segmented-control__item"));
  }

  set(v, fire) {
    v = String(v == null ? "" : v);
    var any = false;
    this.items().forEach((it) => {
      var on = it.getAttribute("data-value") === v;
      any = any || on;
      it.setAttribute("data-state", on ? "active" : "inactive");
      it.setAttribute("aria-selected", String(on));
      it.setAttribute("tabindex", on ? "0" : "-1");
    });
    if (!any) {
      var first = this.items().find((i) => !i.disabled);
      if (first) { first.setAttribute("tabindex", "0"); }
    }
    L.setValue(this.element, v, fire);
  }
});
