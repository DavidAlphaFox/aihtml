/* Behaviour of the input_otp component (designs/04-components.md).
 * Ported from sigil: form/input_otp. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_input.js";

var NS = AH.NS;
var L = AH.lib.input;

function otpSlots($el) { return $el.find(".ah-input-otp__slot"); }

function otpSanitize($el, s, len) {
  var re = $el.attr("data-pattern") === "alphanumeric" ? /[^0-9a-zA-Z]/g : /[^0-9]/g;
  return String(s || "").replace(re, "").slice(0, Math.max(0, len));
}

function otpRead($el) {
  return otpSlots($el).map(function () { return this.value; }).get().join("");
}

function otpFocus($el, i) {
  var $s = otpSlots($el);
  if (!$s.length) { return; }
  var s = $s.get(Math.max(0, Math.min(i, $s.length - 1)));
  s.focus();
  s.select();
}

// Lay a value out over the slots (from the first).
function otpWrite($el, v) {
  otpSlots($el).each(function (i) {
    this.value = v.charAt(i);
  });
}

function otpCommit($el) {
  var $s = otpSlots($el);
  $s.each(function () { this.setAttribute("data-filled", this.value ? "true" : "false"); });
  // The value is the filled prefix: a gap ends it.
  var v = "";
  $s.each(function () {
    if (!this.value) { return false; }
    v += this.value;
  });
  var complete = v.length === $s.length;
  $el.attr("data-complete", complete ? "true" : "false");
  $el.removeAttr("data-invalid");
  if (L.commitValue($el, v) && complete) {
    $el.trigger("ah:complete", [v]);
  }
}

AH.define("input-otp", {
  init: function (el, $el) {
    L.isolate($el, ".ah-input-otp__slot");
    $el.on("input" + NS, ".ah-input-otp__slot", function () {
      var len = otpSlots($el).length;
      var i = parseInt(this.getAttribute("data-index"), 10);
      var v = otpSanitize($el, this.value, len - i);
      if (v.length > 1) {
        // autofill or IME delivered several characters: spread them
        var cur = otpRead($el);
        otpWrite($el, (cur.slice(0, i) + v).slice(0, len));
        otpCommit($el);
        otpFocus($el, i + v.length);
        return;
      }
      this.value = v;
      otpCommit($el);
      if (v) { otpFocus($el, i + 1); }
    });
    $el.on("keydown" + NS, ".ah-input-otp__slot", function (e) {
      var i = parseInt(this.getAttribute("data-index"), 10);
      var n = otpSlots($el).length;
      switch (e.key) {
        case "Backspace":
          e.preventDefault();
          if (this.value) {
            this.value = "";                  // clear this one only
            otpCommit($el);
          } else if (i > 0) {
            otpSlots($el).get(i - 1).value = ""; // back up and clear
            otpCommit($el);
            otpFocus($el, i - 1);
          }
          break;
        case "Delete":
          e.preventDefault();
          this.value = "";
          otpCommit($el);
          break;
        case "ArrowLeft": e.preventDefault(); otpFocus($el, i - 1); break;
        case "ArrowRight": e.preventDefault(); otpFocus($el, i + 1); break;
        case "Home": e.preventDefault(); otpFocus($el, 0); break;
        case "End": e.preventDefault(); otpFocus($el, n - 1); break;
        default:
          // typing over a filled slot replaces it
          if (e.key && e.key.length === 1 && !e.ctrlKey && !e.metaKey &&
              this.value && this.selectionStart === this.selectionEnd) {
            this.select();
          }
      }
    });
    $el.on("paste" + NS, ".ah-input-otp__slot", function (e) {
      e.preventDefault();
      var cd = (e.originalEvent || e).clipboardData;
      var n = otpSlots($el).length;
      var v = otpSanitize($el, cd ? cd.getData("text") : "", n);
      if (!v) { return; }
      otpWrite($el, v);
      otpCommit($el);
      otpFocus($el, Math.min(v.length, n - 1));
    });
    $el.on("focus" + NS, ".ah-input-otp__slot", function () { this.select(); });
    // A click on an empty slot past the first gap goes to the gap.
    $el.on("mousedown" + NS, ".ah-input-otp__slot", function (e) {
      var gap = otpRead($el).length;
      var i = parseInt(this.getAttribute("data-index"), 10);
      if (!this.value && i > gap) {
        e.preventDefault();
        otpFocus($el, gap);
      }
    });
  },
  methods: {
    getValue: function (el, $el) { return $el.attr("data-ah-value") || ""; },
    setValue: function (el, $el, v) {
      otpWrite($el, otpSanitize($el, v, otpSlots($el).length));
      otpCommit($el);
    },
    clear: function (el, $el) { otpWrite($el, ""); otpCommit($el); },
    focus: function (el, $el) { otpFocus($el, otpRead($el).length); },
    // Mark the code wrong (e.g. after the server rejected it).
    invalid: function (el, $el, on) {
      if (on === false) { $el.removeAttr("data-invalid"); } else { $el.attr("data-invalid", "true"); }
    }
  }
});
