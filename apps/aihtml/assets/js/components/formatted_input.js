/* Behaviour of formatted_input (designs/04-components.md), after sigil's
 * form/formatted_input.
 *
 *   formatted-input  an integer (BigInt) typed in radix 2/8/10/16; arrow
 *                    keys and the spin buttons (held: repeat) step it; a
 *                    radix menu (AH.float); data-ah-value stays decimal
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change".
 *
 * The server renders the whole first state, so init only binds events.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function docNS(el) {
    var ns = $.data(el, "ah-docns");
    if (!ns) {
      ns = NS + "ef" + (++seq);
      $.data(el, "ah-docns", ns);
    }
    return ns;
  }

  function setValue($el, v) {
    $el.attr("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

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

  function fmtState(el) { return $.data(el, "ah-fmt"); }

  function fmtClamp(st, b) {
    if (st.min !== null && b < st.min) { return st.min; }
    if (st.max !== null && b > st.max) { return st.max; }
    return b;
  }

  // Show the value in the radix (plain while the field has focus).
  function fmtShow(st) {
    var editing = document.activeElement === st.$input[0];
    var t = bigText(st.value, st.radix, st.upper, st.expo && !editing);
    st.$input.val(t).attr({ "aria-valuenow": st.value.toString(), "aria-valuetext": t });
  }

  // Set the value (clamped); fire change when asked and it changed.
  function fmtSet(el, $el, b, fire) {
    var st = fmtState(el), old = $el.attr("data-ah-value");
    st.value = fmtClamp(st, b);
    fmtShow(st);
    var v = st.value.toString();
    setValue($el, v);
    if (fire && v !== old) { $el.trigger("change", [v]); }
  }

  function fmtStep(el, $el, dir) {
    var st = fmtState(el);
    if (st.$input[0].disabled) { return; }
    fmtSet(el, $el, st.value + st.step * BigInt(dir), true);
    if (st.editing) { st.focusValue = $el.attr("data-ah-value"); }
  }

  function fmtItems(st) { return st.$popup.children(".ah-fmt-popup-item"); }

  function fmtActive(st, $item) {
    fmtItems(st).removeClass("ah-fmt-popup-item-hover");
    $item.addClass("ah-fmt-popup-item-hover");
    st.$input.attr("aria-activedescendant", $item.attr("id"));
  }

  function fmtOpen(el, $el) {
    var st = fmtState(el);
    if (st.open || !st.$popup.length || st.$input[0].disabled) { return; }
    st.open = true;
    st.$popup.addClass("ah-fmt-popup-open");
    st.float = AH.float(st.$popup[0], $el.children(".ah-fmt-input-row")[0], { placement: "bottom", align: "end" });
    st.$btn.attr("aria-expanded", "true");
    fmtActive(st, fmtItems(st).filter(".ah-fmt-popup-item-active"));
    $(document).on("mousedown" + st.ns + "p", function (e) {
      if (!$.contains(el, e.target) && e.target !== el) { fmtClose(el, $el); }
    });
    $el.trigger("ah:open");
  }

  function fmtClose(el, $el) {
    var st = fmtState(el);
    if (!st.open) { return; }
    st.open = false;
    st.$popup.removeClass("ah-fmt-popup-open");
    if (st.float) { st.float.stop(); st.float = null; }
    st.$btn.attr("aria-expanded", "false");
    st.$input.removeAttr("aria-activedescendant");
    fmtItems(st).removeClass("ah-fmt-popup-item-hover");
    $(document).off("mousedown" + st.ns + "p");
    $el.trigger("ah:close");
  }

  function fmtRadix(el, $el, radix) {
    var st = fmtState(el), old = st.radix;
    radix = parseInt(radix, 10);
    if (!RADIX_CHARS[radix]) { return; }
    fmtClose(el, $el);
    if (radix === old) { return; }
    st.radix = radix;
    $el.attr("data-ah-radix", String(radix));
    fmtItems(st).each(function () {
      var on = this.getAttribute("data-radix") === String(radix);
      $(this).toggleClass("ah-fmt-popup-item-active", on).attr("aria-selected", String(on));
    });
    fmtShow(st);
    $el.trigger("ah:radix-change", [radix, old]);
  }

  function stopRepeat(st) {
    if (st.timer) { clearTimeout(st.timer); st.timer = null; }
    if (st.iv) { clearInterval(st.iv); st.iv = null; }
  }

  function startRepeat(st, f, delay, interval) {
    stopRepeat(st);
    f();
    st.timer = setTimeout(function () {
      st.timer = null;
      st.iv = setInterval(f, interval);
    }, delay);
  }

  AH.define("formatted-input", {
    init: function (el, $el) {
      var min = el.getAttribute("data-ah-min"), max = el.getAttribute("data-ah-max");
      var st = {
        ns: docNS(el),
        $input: $el.find("input.ah-fmt-input"),
        $popup: $el.children(".ah-fmt-popup"),
        $btn: $el.find(".ah-fmt-dropdown-btn"),
        radix: parseInt(el.getAttribute("data-ah-radix"), 10) || 10,
        min: min === null ? null : BigInt(min),
        max: max === null ? null : BigInt(max),
        step: BigInt(el.getAttribute("data-ah-step") || "1"),
        upper: el.hasAttribute("data-ah-upper"),
        expo: el.getAttribute("data-ah-notation") === "exponential",
        value: BigInt(el.getAttribute("data-ah-value") || "0"),
        open: false, editing: false, focusValue: null
      };
      $.data(el, "ah-fmt", st);
      var $in = st.$input;

      $in.on("keydown" + NS, function (e) {
        var k = e.key || "", $items, idx;
        if (st.open && (k === "ArrowDown" || k === "ArrowUp")) {
          e.preventDefault();
          $items = fmtItems(st);
          idx = $items.index($items.filter(".ah-fmt-popup-item-hover"));
          idx = (idx + (k === "ArrowDown" ? 1 : -1) + $items.length) % $items.length;
          fmtActive(st, $items.eq(idx));
          return;
        }
        if (st.open && (k === "Enter" || k === " ")) {
          e.preventDefault();
          fmtRadix(el, $el, fmtItems(st).filter(".ah-fmt-popup-item-hover").attr("data-radix"));
          return;
        }
        if (k === "Escape") {
          if (st.open) { e.preventDefault(); fmtClose(el, $el); }
          return;
        }
        if (e.altKey && (k === "ArrowDown" || k === "ArrowUp")) {
          e.preventDefault();
          if (k === "ArrowDown") { fmtOpen(el, $el); } else { fmtClose(el, $el); }
          return;
        }
        if (e.ctrlKey || e.metaKey || e.altKey) { return; }
        if (k === "ArrowUp" || k === "ArrowDown") {
          e.preventDefault();
          fmtSet(el, $el, bigParse($in.val(), st.radix), false);   // what was typed so far
          fmtStep(el, $el, k === "ArrowUp" ? 1 : -1);
          return;
        }
        if (k === "-") {
          if (this.selectionStart !== 0 || $in.val().charAt(0) === "-" && this.selectionEnd === 0) {
            e.preventDefault();
          }
          return;
        }
        if (k.length === 1 && !RADIX_CHARS[st.radix].test(k)) { e.preventDefault(); }
      });
      $in.on("input" + NS, function () {
        var b = bigParse($in.val(), st.radix), v = b.toString();
        st.value = b;
        if (v !== $el.attr("data-ah-value")) {
          setValue($el, v);
          $el.trigger("input", [v]);
        }
      });
      $in.on("focus" + NS, function () {
        $el.addClass("ah-fmt-input-focused");
        st.editing = true;
        st.focusValue = $el.attr("data-ah-value");
        if (st.expo) { fmtShow(st); }
      });
      $in.on("blur" + NS, function () {
        $el.removeClass("ah-fmt-input-focused");
        st.editing = false;
        var before = st.focusValue;
        st.focusValue = null;
        fmtSet(el, $el, bigParse($in.val(), st.radix), false);
        var v = $el.attr("data-ah-value");
        if (before !== null && v !== before) { $el.trigger("change", [v]); }
      });

      $el.on("mousedown" + NS, ".ah-fmt-spin-up, .ah-fmt-spin-down", function (e) {
        if (e.button !== 0 || $in[0].disabled) { return; }
        e.preventDefault();
        var dir = $(this).hasClass("ah-fmt-spin-up") ? 1 : -1;
        if (st.editing) { fmtSet(el, $el, bigParse($in.val(), st.radix), false); }
        startRepeat(st, function () { fmtStep(el, $el, dir); }, 400, 75);
        $(document).on("mouseup" + st.ns + "s", function () {
          stopRepeat(st);
          $(document).off("mouseup" + st.ns + "s");
        });
      });
      $el.on("mousedown" + NS, ".ah-fmt-dropdown-btn", function (e) {
        e.preventDefault();
        if (st.open) { fmtClose(el, $el); } else { fmtOpen(el, $el); }
      });
      st.$popup.on("mousedown" + NS, ".ah-fmt-popup-item", function (e) {
        e.preventDefault();
        fmtRadix(el, $el, this.getAttribute("data-radix"));
      });
    },
    destroy: function (el) {
      var st = fmtState(el);
      if (!st) { return; }
      stopRepeat(st);
      if (st.float) { st.float.stop(); st.float = null; }
      st.$popup.off(NS);
      $(document).off(st.ns + "p").off(st.ns + "s");
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = fmtState(el);
        fmtSet(el, $el, bigParse(String(v), 10), false);
        if (st.editing) { st.focusValue = $el.attr("data-ah-value"); }
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      setRadix: function (el, $el, radix) { fmtRadix(el, $el, radix); },
      getRadix: function (el) { return fmtState(el).radix; },
      open: function (el, $el) { fmtOpen(el, $el); },
      close: function (el, $el) { fmtClose(el, $el); }
    }
  });
})(window.jQuery, window.AH);
