/* Behaviour of the input_otp component (designs/04-components.md).
 * Ported from sigil: form/input_otp. The root fires "change" (no detail)
 * and, when every slot is filled, "ah:complete" (detail: the code). */
import AH from "../core.js";
import "./_lib_input.js";

var L = AH.lib.input;

AH.register("input-otp", class extends AH.Controller {
  setup() {
    this.delegate("input", ".ah-input-otp__slot", (e, slot) => {
      var len = this.slots().length;
      var i = parseInt(slot.getAttribute("data-index"), 10);
      var v = this.sanitize(slot.value, len - i);
      if (v.length > 1) {
        // autofill or IME delivered several characters: spread them
        var cur = this.read();
        this.write((cur.slice(0, i) + v).slice(0, len));
        this.commit();
        this.focusAt(i + v.length);
        return;
      }
      slot.value = v;
      this.commit();
      if (v) { this.focusAt(i + 1); }
    });
    // after the handler above: the slots' events stop at the root
    L.isolate(this, ".ah-input-otp__slot");
    this.delegate("keydown", ".ah-input-otp__slot", (e, slot) => {
      var i = parseInt(slot.getAttribute("data-index"), 10);
      var n = this.slots().length;
      switch (e.key) {
        case "Backspace":
          e.preventDefault();
          if (slot.value) {
            slot.value = "";                  // clear this one only
            this.commit();
          } else if (i > 0) {
            this.slots()[i - 1].value = "";   // back up and clear
            this.commit();
            this.focusAt(i - 1);
          }
          break;
        case "Delete":
          e.preventDefault();
          slot.value = "";
          this.commit();
          break;
        case "ArrowLeft": e.preventDefault(); this.focusAt(i - 1); break;
        case "ArrowRight": e.preventDefault(); this.focusAt(i + 1); break;
        case "Home": e.preventDefault(); this.focusAt(0); break;
        case "End": e.preventDefault(); this.focusAt(n - 1); break;
        default:
          // typing over a filled slot replaces it
          if (e.key && e.key.length === 1 && !e.ctrlKey && !e.metaKey &&
              slot.value && slot.selectionStart === slot.selectionEnd) {
            slot.select();
          }
      }
    });
    this.delegate("paste", ".ah-input-otp__slot", (e) => {
      e.preventDefault();
      var cd = e.clipboardData;
      var n = this.slots().length;
      var v = this.sanitize(cd ? cd.getData("text") : "", n);
      if (!v) { return; }
      this.write(v);
      this.commit();
      this.focusAt(Math.min(v.length, n - 1));
    });
    this.listen(this.element, "focusin", (e) => {
      if (e.target.classList && e.target.classList.contains("ah-input-otp__slot")) { e.target.select(); }
    });
    // A click on an empty slot past the first gap goes to the gap.
    this.delegate("mousedown", ".ah-input-otp__slot", (e, slot) => {
      var gap = this.read().length;
      var i = parseInt(slot.getAttribute("data-index"), 10);
      if (!slot.value && i > gap) {
        e.preventDefault();
        this.focusAt(gap);
      }
    });
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  setValue(v) {
    this.write(this.sanitize(v, this.slots().length));
    this.commit();
  }
  clear() { this.write(""); this.commit(); }
  focus() { this.focusAt(this.read().length); }
  // Mark the code wrong (e.g. after the server rejected it).
  invalid(on) {
    if (on === false) { this.element.removeAttribute("data-invalid"); }
    else { this.element.setAttribute("data-invalid", "true"); }
  }

  slots() { return Array.from(this.element.querySelectorAll(".ah-input-otp__slot")); }

  sanitize(s, len) {
    var re = this.element.getAttribute("data-pattern") === "alphanumeric" ? /[^0-9a-zA-Z]/g : /[^0-9]/g;
    return String(s || "").replace(re, "").slice(0, Math.max(0, len));
  }

  read() { return this.slots().map((s) => s.value).join(""); }

  focusAt(i) {
    var s = this.slots();
    if (!s.length) { return; }
    var slot = s[Math.max(0, Math.min(i, s.length - 1))];
    slot.focus();
    slot.select();
  }

  // Lay a value out over the slots (from the first).
  write(v) {
    this.slots().forEach((s, i) => { s.value = v.charAt(i); });
  }

  commit() {
    var el = this.element, s = this.slots();
    s.forEach((x) => { x.setAttribute("data-filled", x.value ? "true" : "false"); });
    // The value is the filled prefix: a gap ends it.
    var v = "";
    for (var i = 0; i < s.length && s[i].value; i++) { v += s[i].value; }
    var complete = v.length === s.length;
    el.setAttribute("data-complete", complete ? "true" : "false");
    el.removeAttribute("data-invalid");
    if (L.commitValue(el, v) && complete) {
      this.fire("ah:complete", v);
    }
  }
});
