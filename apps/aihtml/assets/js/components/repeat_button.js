/* Behaviour of repeat_button (designs/04-components.md), after sigil's
 * form/repeat_button.
 *
 *   repeat-button    a native click on press, then every interval ms after
 *                    delay ms while held; the browser's click on release
 *                    is dropped
 *
 * The server renders the whole first state, so setup only binds events.
 */
import AH from "../core.js";

AH.register("repeat-button", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.active = false;
    this.swallow = false;
    this.own = false;
    this.timer = null;
    this.iv = null;
    var delay = parseInt(el.getAttribute("data-ah-delay"), 10);
    this.interval = parseInt(el.getAttribute("data-ah-interval"), 10) || 50;
    this.delay = isNaN(delay) ? 300 : delay;
    var release = () => { this.release(); };
    this.listen(el, "mousedown", (e) => { if (e.button === 0) { this.press(); } });
    this.listen(el, "touchstart", (e) => {
      e.preventDefault();                   // no emulated mouse events, no click
      this.press();
    }, { passive: false });
    ["mouseup", "mouseleave", "touchend", "touchcancel", "blur"].forEach((t) => {
      this.listen(el, t, release);
    });
    this.listen(el, "keydown", (e) => {
      if (e.key !== "Enter" && e.key !== " ") { return; }
      e.preventDefault();
      this.press();                         // auto-repeated keydowns are ignored
    });
    this.listen(el, "keyup", (e) => {
      if (e.key === "Enter" || e.key === " ") { e.preventDefault(); this.release(); }
    });
    this.listen(el, "click", (e) => {
      if (!this.own && (this.swallow || this.active)) {
        e.preventDefault();
        e.stopImmediatePropagation();
      }
    });
  }

  teardown() {
    this.active = false;
    this.stopRepeat();
  }

  // methods (aihtml_action:call/4, AH.invoke)
  stop() { this.release(); }

  stopRepeat() {
    if (this.timer) { clearTimeout(this.timer); this.timer = null; }
    if (this.iv) { clearInterval(this.iv); this.iv = null; }
  }

  // One repetition: a native click the page's listeners see.
  repeat() {
    var el = this.element;
    if (el.disabled) { this.release(); return; }
    this.own = true;
    try { el.click(); } finally { this.own = false; }
  }

  press() {
    if (this.element.disabled || this.active) { return; }
    this.active = true;
    this.element.classList.add("ah-btn-pressed");
    this.stopRepeat();
    this.repeat();
    this.timer = setTimeout(() => {
      this.timer = null;
      this.iv = setInterval(() => { this.repeat(); }, this.interval);
    }, this.delay);
  }

  release() {
    if (!this.active) { return; }
    this.active = false;
    this.stopRepeat();
    this.element.classList.remove("ah-btn-pressed");
    // the click the browser sends for this release is not another repetition
    this.swallow = true;
    setTimeout(() => { this.swallow = false; }, 0);
  }
});
