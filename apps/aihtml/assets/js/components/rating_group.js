/* rating behaviour (designs/04-components.md): a value-bearing custom
 * control: data-ah-value, the hidden input and a "change" on the root;
 * "ah:hover" (detail: the value, or null) while the pointer previews a
 * value. Methods never fire change.
 */
import AH from "../core.js";

AH.register("rating", class extends AH.Controller {
  setup() {
    this.delegate("mousemove", ".ah-rating__star", (e, star) => {
      if (!this.state().live) { return; }
      const v = this.valueAt(star, e);
      this.paint(v);
      this.fire("ah:hover", v);
    });
    this.listen(this.element, "mouseleave", () => {
      const s = this.state();
      if (!s.live) { return; }
      this.paint(s.value);
      this.fire("ah:hover", null);
    });
    this.delegate("click", ".ah-rating__star", (e, star) => {
      const s = this.state();
      if (!s.live) { return; }
      const v = this.valueAt(star, e);
      this.set(s.clear && v === s.value ? 0 : v, true);
    });
    this.delegate("keydown", ".ah-rating__star", (e) => {
      const s = this.state();
      if (!s.live) { return; }
      const d = { ArrowRight: 1, ArrowUp: 1, ArrowLeft: -1, ArrowDown: -1 }[e.key];
      if (!d) { return; }
      e.preventDefault();
      this.set(s.value + d * s.step, true);
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) { this.set(v, false); }
  getValue() { return this.state().value; }

  state() {
    const el = this.element;
    return {
      value: parseFloat(el.getAttribute("data-ah-value")) || 0,
      max: parseInt(el.getAttribute("data-ah-max"), 10) || 5,
      step: el.getAttribute("data-precision") === "0.5" ? 0.5 : 1,
      live: el.getAttribute("data-readonly") !== "true" &&
            el.getAttribute("data-disabled") !== "true",
      clear: el.getAttribute("data-allow-clear") !== "false"
    };
  }

  stars() { return this.element.querySelectorAll(".ah-rating__star"); }

  paint(v) {
    this.stars().forEach((star, i) => {
      const r = Math.max(0, Math.min(1, v - i));
      star.querySelectorAll(".ah-rating__filled").forEach((f) => { f.style.width = (r * 100) + "%"; });
    });
  }

  set(v, fire) {
    const s = this.state();
    v = Math.max(0, Math.min(s.max, Math.round(Number(v) / s.step) * s.step || 0));
    const changed = v !== s.value;
    this.element.setAttribute("data-ah-value", String(v));
    const hidden = this.element.querySelector(":scope > input[type=hidden]");
    if (hidden) { hidden.value = String(v); }
    this.stars().forEach((star, i) => {
      star.setAttribute("aria-checked", v >= i + 1 ? "true" : "false");
    });
    this.paint(v);
    if (fire && changed) { this.fire("change"); }
  }

  // The value under the pointer; a keyboard click (detail 0) takes the
  // whole star.
  valueAt(star, e) {
    const s = this.state();
    const idx = parseInt(star.getAttribute("data-index"), 10);
    if (s.step === 0.5 && e.detail !== 0 && e.clientX !== undefined) {
      const rect = star.getBoundingClientRect();
      return idx + ((e.clientX - rect.left) / rect.width <= 0.5 ? 0.5 : 1);
    }
    return idx + 1;
  }
});
