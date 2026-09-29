/* Behaviour of the number_input component (designs/04-components.md).
 * Ported from sigil: form/number_input. Stepping fires native "input"
 * and "change" on the inner <input>, as typing does. */
import AH from "../core.js";
import "./_lib_input.js";

var L = AH.lib.input;

function clamp(v, o) {
  if (o.min !== null) { v = Math.max(o.min, v); }
  if (o.max !== null) { v = Math.min(o.max, v); }
  return v;
}

function parseNum(text) {
  var s = String(text == null ? "" : text).replace(/,/g, "").trim();
  if (s === "") { return null; }
  var n = parseFloat(s);
  return isNaN(n) ? null : n;
}

AH.register("number-input", class extends AH.Controller {
  setup() {
    var el = this.element, input = this.field();
    this.spin = null;
    if (!input) { return; }
    L.focusShell(this, input, "ah-numinput");
    this.listen(input, "keydown", (e) => {
      var k = e.key || "";
      if (e.ctrlKey || e.metaKey || e.altKey) { return; }
      if (k === "ArrowUp" || k === "ArrowDown") {
        e.preventDefault();
        this.step(k === "ArrowUp" ? 1 : -1);
      } else if (k === "PageUp" || k === "PageDown") {
        e.preventDefault();
        this.step((k === "PageUp" ? 1 : -1) * 10);
      } else if (k === "-") {
        // a minus sign only at the start, once
        if (input.selectionStart !== 0 || input.value.indexOf("-") >= 0) { e.preventDefault(); }
      } else if (k === ".") {
        if (this.opts().decimals === 0 || input.value.indexOf(".") >= 0) { e.preventDefault(); }
      } else if (k.length === 1 && !/[0-9]/.test(k)) {
        e.preventDefault();
      }
    });
    // Typing ends in a native change (on blur or Enter): normalise first,
    // this handler runs before the delegated action handlers.
    this.listen(input, "change", () => {
      this.write(parseNum(input.value));
    });
    this.listen(input, "wheel", (e) => {
      if (document.activeElement !== input) { return; }
      e.preventDefault();
      this.step(e.deltaY < 0 ? 1 : -1);
    }, { passive: false });
    // Spin buttons: step, then repeat after 400ms every 75ms (sigil).
    this.delegate("mousedown", ".ah-numinput-spin-up, .ah-numinput-spin-down", (e, btn) => {
      if (e.button !== 0) { return; }
      e.preventDefault();
      var dir = btn.classList.contains("ah-numinput-spin-up") ? 1 : -1;
      this.stopRepeat();
      this.step(dir);
      var t = {};
      t.delay = setTimeout(() => {
        t.every = setInterval(() => { this.step(dir); }, 75);
      }, 400);
      this.spin = t;
      if (document.activeElement !== input) { input.focus(); }
    });
    el.querySelectorAll(".ah-numinput-spin").forEach((s) => {
      this.listen(s, "mouseleave", () => { this.stopRepeat(); });
    });
    this.listen(document, "mouseup", () => { this.stopRepeat(); });
  }

  teardown() { this.stopRepeat(); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return parseNum(this.field().value); }
  setValue(v) {
    var input = this.field(), before = input.value;
    this.write(v === null || v === undefined ? null : parseNum(v));
    if (input.value !== before) { L.emit(input, "change"); }
  }
  stepUp() { this.step(1); }
  stepDown() { this.step(-1); }
  clear() {
    var input = this.field(), before = input.value;
    this.write(null);
    if (input.value !== before) { L.emit(input, "change"); }
  }
  focus() { this.field().focus(); }

  field() { return this.element.querySelector("input.ah-numinput-input"); }

  opts() {
    var el = this.element;
    var num = function (a) {
      var v = el.getAttribute(a);
      return v === null || v === "" ? null : parseFloat(v);
    };
    return {
      min: num("data-min"),
      max: num("data-max"),
      step: num("data-step") || 1,
      decimals: parseInt(el.getAttribute("data-decimals") || "0", 10),
      allowNull: el.getAttribute("data-allow-null") !== "false"
    };
  }

  // Write a (clamped, formatted) value; returns the value written.
  write(v) {
    var o = this.opts(), input = this.field();
    if (v === null) {
      v = o.allowNull ? null : clamp(0, o);
    } else {
      v = clamp(v, o);
    }
    input.value = v === null ? "" : v.toFixed(o.decimals);
    if (v === null) { input.removeAttribute("aria-valuenow"); } else { input.setAttribute("aria-valuenow", v); }
    L.syncLabel(this.element, "ah-numinput", v !== null, document.activeElement === input);
    return v;
  }

  step(dir) {
    var input = this.field();
    if (!input || input.disabled || input.readOnly) { return; }
    var o = this.opts();
    var before = input.value;
    var cur = parseNum(before);
    var next = parseFloat(((cur === null ? 0 : cur) + o.step * dir).toFixed(o.decimals));
    this.write(next);
    if (input.value !== before) {
      L.emit(input, "input");
      L.emit(input, "change");
    }
  }

  stopRepeat() {
    var t = this.spin;
    if (t) { clearTimeout(t.delay); clearInterval(t.every); }
    this.spin = null;
  }
});
