/* Behaviour of formatted_input (designs/04-components.md), after sigil's
 * form/formatted_input.
 *
 *   formatted-input  an integer (BigInt) typed in radix 2/8/10/16; arrow
 *                    keys and the spin buttons (held: repeat) step it; a
 *                    radix menu (AH.float); data-ah-value stays decimal
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change" (detail: the decimal value), "ah:open" / "ah:close"
 * (no detail) and "ah:radix-change" (detail: {radix, old}).
 *
 * The server renders the whole first state, so setup only binds events.
 */
import AH from "../core.js";

// ------------------------------------------------------------------
// formatted-input
// ------------------------------------------------------------------

var RADIX_PREFIX = { 2: "0b", 8: "0o", 10: "", 16: "0x" };
var RADIX_CHARS = { 2: /^[01]$/, 8: /^[0-7]$/, 10: /^[0-9]$/, 16: /^[0-9a-f]$/i };

// Text in a radix -> BigInt; empty or invalid -> 0n.
function bigParse(s, radix) {
  s = String(s == null ? "" : s).trim();
  if (!s) { return BigInt(0); }
  var neg = s.charAt(0) === "-";
  if (neg) { s = s.slice(1); }
  if (!s) { s = "0"; }
  try {
    var b = BigInt(RADIX_PREFIX[radix] + s);
    return neg ? -b : b;
  } catch (e) {
    return BigInt(0);
  }
}

function bigText(b, radix, upper, expo) {
  var s = b.toString(radix);
  if (upper) { s = s.toUpperCase(); }
  if (expo && radix === 10) {
    var neg = s.charAt(0) === "-", abs = neg ? s.slice(1) : s;
    if (abs.length > 1) { s = (neg ? "-" : "") + abs.charAt(0) + "." + abs.slice(1) + "e+" + (abs.length - 1); }
  }
  return s;
}

function writeValue(el, v) {
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll(":scope > input[type=hidden]").forEach(function (h) { h.value = v; });
}

AH.register("formatted-input", class extends AH.Controller {
  setup() {
    var el = this.element;
    var min = el.getAttribute("data-ah-min"), max = el.getAttribute("data-ah-max");
    var st = this.st = {
      input: el.querySelector("input.ah-fmt-input"),
      popup: el.querySelector(":scope > .ah-fmt-popup"),
      btn: el.querySelector(".ah-fmt-dropdown-btn"),
      radix: parseInt(el.getAttribute("data-ah-radix"), 10) || 10,
      min: min === null ? null : BigInt(min),
      max: max === null ? null : BigInt(max),
      step: BigInt(el.getAttribute("data-ah-step") || "1"),
      upper: el.hasAttribute("data-ah-upper"),
      expo: el.getAttribute("data-ah-notation") === "exponential",
      value: BigInt(el.getAttribute("data-ah-value") || "0"),
      open: false, editing: false, focusValue: null,
      timer: null, iv: null, float: null, spinUp: null
    };
    var inp = st.input;
    if (!inp) { return; }

    this.listen(inp, "keydown", (e) => {
      var k = e.key || "", items, idx;
      if (st.open && (k === "ArrowDown" || k === "ArrowUp")) {
        e.preventDefault();
        items = this.items();
        idx = items.findIndex((i) => i.classList.contains("ah-fmt-popup-item-hover"));
        idx = (idx + (k === "ArrowDown" ? 1 : -1) + items.length) % items.length;
        this.activate(items[idx]);
        return;
      }
      if (st.open && (k === "Enter" || k === " ")) {
        e.preventDefault();
        var hover = this.items().find((i) => i.classList.contains("ah-fmt-popup-item-hover"));
        this.radix(hover ? hover.getAttribute("data-radix") : undefined);
        return;
      }
      if (k === "Escape") {
        if (st.open) { e.preventDefault(); this.closeMenu(); }
        return;
      }
      if (e.altKey && (k === "ArrowDown" || k === "ArrowUp")) {
        e.preventDefault();
        if (k === "ArrowDown") { this.openMenu(); } else { this.closeMenu(); }
        return;
      }
      if (e.ctrlKey || e.metaKey || e.altKey) { return; }
      if (k === "ArrowUp" || k === "ArrowDown") {
        e.preventDefault();
        this.set(bigParse(inp.value, st.radix), false);   // what was typed so far
        this.stepBy(k === "ArrowUp" ? 1 : -1);
        return;
      }
      if (k === "-") {
        if (inp.selectionStart !== 0 || inp.value.charAt(0) === "-" && inp.selectionEnd === 0) {
          e.preventDefault();
        }
        return;
      }
      if (k.length === 1 && !RADIX_CHARS[st.radix].test(k)) { e.preventDefault(); }
    });
    this.listen(inp, "input", () => {
      var b = bigParse(inp.value, st.radix), v = b.toString();
      st.value = b;
      if (v !== el.getAttribute("data-ah-value")) {
        writeValue(el, v);
        this.fire("input", v);
      }
    });
    this.listen(inp, "focus", () => {
      el.classList.add("ah-fmt-input-focused");
      st.editing = true;
      st.focusValue = el.getAttribute("data-ah-value");
      if (st.expo) { this.show(); }
    });
    this.listen(inp, "blur", () => {
      el.classList.remove("ah-fmt-input-focused");
      st.editing = false;
      var before = st.focusValue;
      st.focusValue = null;
      this.set(bigParse(inp.value, st.radix), false);
      var v = el.getAttribute("data-ah-value");
      if (before !== null && v !== before) { this.fire("change", v); }
    });

    this.delegate("mousedown", ".ah-fmt-spin-up, .ah-fmt-spin-down", (e, spin) => {
      if (e.button !== 0 || inp.disabled) { return; }
      e.preventDefault();
      var dir = spin.classList.contains("ah-fmt-spin-up") ? 1 : -1;
      if (st.editing) { this.set(bigParse(inp.value, st.radix), false); }
      this.startRepeat(() => { this.stepBy(dir); }, 400, 75);
      if (!st.spinUp) {
        st.spinUp = new AbortController();
        var stop = () => {
          this.stopRepeat();
          if (st.spinUp) { st.spinUp.abort(); st.spinUp = null; }
        };
        document.addEventListener("mouseup", stop, { signal: st.spinUp.signal });
      }
    });
    this.delegate("mousedown", ".ah-fmt-dropdown-btn", (e) => {
      e.preventDefault();
      if (st.open) { this.closeMenu(); } else { this.openMenu(); }
    });
    if (st.popup) {
      // the popup floats (AH.float) but stays inside the root
      this.delegate("mousedown", ".ah-fmt-popup-item", (e, item) => {
        e.preventDefault();
        this.radix(item.getAttribute("data-radix"));
      }, st.popup);
    }
    this.listen(document, "mousedown", (e) => {
      if (st.open && !el.contains(e.target)) { this.closeMenu(); }
    });
  }

  teardown() {
    var st = this.st;
    this.stopRepeat();
    if (st.spinUp) { st.spinUp.abort(); st.spinUp = null; }
    if (st.float) { st.float.stop(); st.float = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) {
    this.set(bigParse(String(v), 10), false);
    if (this.st.editing) { this.st.focusValue = this.element.getAttribute("data-ah-value"); }
  }
  getValue() { return this.element.getAttribute("data-ah-value"); }
  setRadix(radix) { this.radix(radix); }
  getRadix() { return this.st.radix; }
  open() { this.openMenu(); }
  close() { this.closeMenu(); }

  clamp(b) {
    var st = this.st;
    if (st.min !== null && b < st.min) { return st.min; }
    if (st.max !== null && b > st.max) { return st.max; }
    return b;
  }

  // Show the value in the radix (plain while the field has focus).
  show() {
    var st = this.st;
    var editing = document.activeElement === st.input;
    var t = bigText(st.value, st.radix, st.upper, st.expo && !editing);
    st.input.value = t;
    st.input.setAttribute("aria-valuenow", st.value.toString());
    st.input.setAttribute("aria-valuetext", t);
  }

  // Set the value (clamped); fire change when asked and it changed.
  set(b, fire) {
    var st = this.st, el = this.element, old = el.getAttribute("data-ah-value");
    st.value = this.clamp(b);
    this.show();
    var v = st.value.toString();
    writeValue(el, v);
    if (fire && v !== old) { this.fire("change", v); }
  }

  stepBy(dir) {
    var st = this.st;
    if (st.input.disabled) { return; }
    this.set(st.value + st.step * BigInt(dir), true);
    if (st.editing) { st.focusValue = this.element.getAttribute("data-ah-value"); }
  }

  items() {
    return this.st.popup
      ? Array.from(this.st.popup.children).filter((i) => i.classList.contains("ah-fmt-popup-item"))
      : [];
  }

  activate(item) {
    this.items().forEach((i) => { i.classList.remove("ah-fmt-popup-item-hover"); });
    if (!item) { this.st.input.removeAttribute("aria-activedescendant"); return; }
    item.classList.add("ah-fmt-popup-item-hover");
    this.st.input.setAttribute("aria-activedescendant", item.id);
  }

  openMenu() {
    var st = this.st, el = this.element;
    if (st.open || !st.popup || st.input.disabled) { return; }
    st.open = true;
    st.popup.classList.add("ah-fmt-popup-open");
    st.float = AH.float(st.popup, el.querySelector(":scope > .ah-fmt-input-row"),
                        { placement: "bottom", align: "end" });
    if (st.btn) { st.btn.setAttribute("aria-expanded", "true"); }
    this.activate(this.items().find((i) => i.classList.contains("ah-fmt-popup-item-active")));
    this.fire("ah:open");
  }

  closeMenu() {
    var st = this.st;
    if (!st.open) { return; }
    st.open = false;
    st.popup.classList.remove("ah-fmt-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    if (st.btn) { st.btn.setAttribute("aria-expanded", "false"); }
    st.input.removeAttribute("aria-activedescendant");
    this.items().forEach((i) => { i.classList.remove("ah-fmt-popup-item-hover"); });
    this.fire("ah:close");
  }

  radix(radix) {
    var st = this.st, old = st.radix;
    radix = parseInt(radix, 10);
    if (!RADIX_CHARS[radix]) { return; }
    this.closeMenu();
    if (radix === old) { return; }
    st.radix = radix;
    this.element.setAttribute("data-ah-radix", String(radix));
    this.items().forEach((i) => {
      var on = i.getAttribute("data-radix") === String(radix);
      i.classList.toggle("ah-fmt-popup-item-active", on);
      i.setAttribute("aria-selected", String(on));
    });
    this.show();
    this.fire("ah:radix-change", { radix: radix, old: old });
  }

  stopRepeat() {
    var st = this.st;
    if (st.timer) { clearTimeout(st.timer); st.timer = null; }
    if (st.iv) { clearInterval(st.iv); st.iv = null; }
  }

  startRepeat(f, delay, interval) {
    var st = this.st;
    this.stopRepeat();
    f();
    st.timer = setTimeout(() => {
      st.timer = null;
      st.iv = setInterval(f, interval);
    }, delay);
  }
});
