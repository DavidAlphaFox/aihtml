/* Behaviours of the form_entry components (designs/04-components.md),
 * after sigil's form/masked_input, formatted_input, range_selector and
 * repeat_button.
 *
 *   masked-input     only characters that fit the mask are typed, deleted
 *                    or pasted; input on each edit, change on blur
 *   formatted-input  an integer (BigInt) typed in radix 2/8/10/16; arrow
 *                    keys and the spin buttons (held: repeat) step it; a
 *                    radix menu (AH.float); data-ah-value stays decimal
 *   range-selector   drag a marker or the bar between them; the markers
 *                    are ARIA sliders; input while dragging, change after
 *   repeat-button    click on press, then every interval ms after delay ms
 *                    while held; the browser's click on release is dropped
 *
 * Value-bearing roots keep data-ah-value and their hidden input in step
 * and fire "input" / "change" on the root. The server renders the whole
 * first state, so init only binds events.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var seq = 0;

  function docNS(el) {
    var ns = $.data(el, "ah-docns");
    if (!ns) {
      ns = NS + "e" + (++seq);
      $.data(el, "ah-docns", ns);
    }
    return ns;
  }

  function setValue($el, v) {
    $el.attr("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  // ------------------------------------------------------------------
  // masked-input
  // ------------------------------------------------------------------
  //
  // The mask is a list of positions {re, ch} (editable, ch null when
  // empty) or {lit} (a literal), parsed as aihtml_form_entry:parse_mask.

  var MASK_RE = { "9": "\\d", "0": "\\d", "#": "[\\d|+|-]", "A": "\\w", "a": "\\w",
                  "L": "[a-zA-Z]", "l": "[a-zA-Z]", "c": ".", "C": "." };

  function maskParse(mask) {
    var items = [], chars = Array.from(mask || ""), i, j;
    for (i = 0; i < chars.length; i++) {
      if (chars[i] === "[") {
        j = chars.indexOf("]", i);
        if (j < 0) { j = chars.length - 1; }
        items.push({ re: new RegExp("^(?:(" + chars.slice(i, j + 1).join("") + "))$", "i"), ch: null });
        i = j;
      } else if (MASK_RE[chars[i]]) {
        items.push({ re: new RegExp("^(?:" + MASK_RE[chars[i]] + ")$", "i"), ch: null });
      } else {
        items.push({ lit: chars[i] });
      }
    }
    return items;
  }

  // Fill the editable positions from text, as the server does.
  function maskFill(items, text) {
    var cs = Array.from(text == null ? "" : String(text)), k = 0;
    items.forEach(function (it) {
      if (it.lit != null) {
        if (cs[k] === it.lit) { k++; }
        return;
      }
      it.ch = null;
      while (k < cs.length && !it.re.test(cs[k])) { k++; }
      if (k < cs.length) { it.ch = cs[k++]; }
    });
    return items;
  }

  function maskState(el) { return $.data(el, "ah-mask"); }

  function maskDisplay(st) {
    return st.items.map(function (it) {
      return it.lit != null ? it.lit : (it.ch == null ? st.prompt : it.ch);
    }).join("");
  }

  function maskRaw(st) {
    return st.items.map(function (it) { return it.lit == null && it.ch != null ? it.ch : ""; }).join("");
  }

  function maskValue(st) {
    var raw = maskRaw(st);
    return st.literals ? (raw ? maskDisplay(st) : "") : raw;
  }

  function editable(st, i) { return i >= 0 && i < st.items.length && st.items[i].lit == null; }
  function nextEditable(st, i) { for (; i < st.items.length; i++) { if (editable(st, i)) { return i; } } return -1; }
  function prevEditable(st, i) { for (i--; i >= 0; i--) { if (editable(st, i)) { return i; } } return -1; }
  function firstEmpty(st) {
    for (var i = 0; i < st.items.length; i++) { if (editable(st, i) && st.items[i].ch == null) { return i; } }
    return -1;
  }

  // Positions are characters; the input's selection counts UTF-16 units.
  function unitsTo(st, pos) {
    var n = 0;
    for (var i = 0; i < pos && i < st.items.length; i++) {
      var it = st.items[i];
      n += (it.lit != null ? it.lit : (it.ch == null ? st.prompt : it.ch)).length;
    }
    return n;
  }
  function posOf(st, units) {
    var n = 0;
    for (var i = 0; i < st.items.length; i++) {
      if (n >= units) { return i; }
      var it = st.items[i];
      n += (it.lit != null ? it.lit : (it.ch == null ? st.prompt : it.ch)).length;
    }
    return st.items.length;
  }
  function selection(st) {
    var inp = st.input;
    return { start: posOf(st, inp.selectionStart), end: posOf(st, inp.selectionEnd) };
  }
  function setCursor(st, pos) {
    var u = unitsTo(st, Math.max(0, Math.min(pos, st.items.length)));
    try { st.input.setSelectionRange(u, u); } catch (e) { /* not focused */ }
  }

  function clearRange(st, a, b) {
    for (var i = a; i < b; i++) { if (editable(st, i)) { st.items[i].ch = null; } }
  }

  // What the field shows: the mask, or nothing for an empty field with a
  // floating label (the label stands there) while it has no focus.
  function maskText(st) {
    return st.$label.length && document.activeElement !== st.input && maskRaw(st) === ""
      ? "" : maskDisplay(st);
  }

  // Show the items; fire input when the value changed.
  function maskShow(el, $el, cursor, fire) {
    var st = maskState(el), old = $el.attr("data-ah-value"), v = maskValue(st);
    st.input.value = maskText(st);
    setValue($el, v);
    if (cursor != null) { setCursor(st, cursor); }
    if (st.$label.length && document.activeElement !== st.input) {
      st.$label.toggleClass("ah-masked-input-label-float", maskRaw(st) !== "");
    }
    if (fire && v !== old) { $el.trigger("input", [v]); }
  }

  // Type one character (sigil's do-insert-char!: into the next editable
  // position if it fits, else jump past the literal it names) or paste a
  // text (sigil's do-paste!: characters that do not fit are skipped).
  function maskInsert(el, $el, text) {
    var st = maskState(el), sel = selection(st), chars = Array.from(text), pos = sel.start, i, j;
    var cleared = sel.end > sel.start;
    if (cleared) { clearRange(st, sel.start, sel.end); }
    if (chars.length === 1) {
      i = nextEditable(st, pos);
      if (i >= 0 && st.items[i].re.test(chars[0])) {
        st.items[i].ch = chars[0];
        j = nextEditable(st, i + 1);
        maskShow(el, $el, j < 0 ? st.items.length : j, true);
        return;
      }
      for (j = pos; j < st.items.length; j++) {
        if (st.items[j].lit === chars[0]) {
          if (cleared) { maskShow(el, $el, j + 1, true); } else { setCursor(st, j + 1); }
          return;
        }
      }
      if (cleared) { maskShow(el, $el, sel.start, true); }
      return;
    }
    var k = 0;
    while (k < chars.length && pos < st.items.length) {
      if (!editable(st, pos)) { pos++; continue; }
      if (st.items[pos].re.test(chars[k])) { st.items[pos].ch = chars[k]; pos++; }
      k++;
    }
    i = firstEmpty(st);
    maskShow(el, $el, i < 0 ? st.items.length : i, true);
  }

  function maskDelete(el, $el, back) {
    var st = maskState(el), sel = selection(st), i;
    if (sel.end > sel.start) {
      clearRange(st, sel.start, sel.end);
      maskShow(el, $el, sel.start, true);
    } else if (back) {
      i = prevEditable(st, sel.start);
      if (i >= 0) { st.items[i].ch = null; maskShow(el, $el, i, true); }
    } else {
      i = nextEditable(st, sel.start);
      if (i >= 0) { st.items[i].ch = null; maskShow(el, $el, sel.start, true); }
    }
  }

  function maskBlocked(st) { return st.input.disabled || st.input.readOnly; }

  AH.define("masked-input", {
    init: function (el, $el) {
      var $in = $el.children("input.ah-masked-input");
      var st = {
        input: $in[0],
        $label: $el.children(".ah-masked-input-label"),
        prompt: el.getAttribute("data-ah-prompt") || "_",
        literals: el.hasAttribute("data-ah-literals"),
        items: maskParse(el.getAttribute("data-ah-mask")),
        focusValue: null
      };
      $.data(el, "ah-mask", st);
      // the server filled the mask: read the positions back
      Array.from($in.val()).forEach(function (c, i) {
        if (editable(st, i) && c !== st.prompt) { st.items[i].ch = c; }
      });

      $in.on("keydown" + NS, function (e) {
        var k = e.key || "";
        if (e.keyCode === 229) { return; }                       // IME: beforeinput
        if (e.ctrlKey || e.metaKey || e.altKey) {
          if ((k === "x" || k === "X") && !maskBlocked(st)) {
            var sel = selection(st);
            // after the browser copied the selection
            setTimeout(function () {
              if (st.input.value === maskDisplay(st) && sel.end > sel.start) {
                clearRange(st, sel.start, sel.end);
                maskShow(el, $el, sel.start, true);
              }
            }, 10);
          }
          return;
        }
        if (k === "Backspace" || k === "Delete") {
          e.preventDefault();
          if (!maskBlocked(st)) { maskDelete(el, $el, k === "Backspace"); }
        } else if (k.length > 0 && Array.from(k).length === 1) {
          e.preventDefault();
          if (!maskBlocked(st)) { maskInsert(el, $el, k); }
        }
      });
      $in.on("beforeinput" + NS, function (je) {
        var e = je.originalEvent;
        if (!e) { return; }
        switch (e.inputType) {
          case "insertText": case "insertReplacementText": case "insertCompositionText":
            e.preventDefault();
            if (e.data && !maskBlocked(st)) { maskInsert(el, $el, e.data); }
            break;
          case "insertFromPaste": case "insertFromDrop":
            e.preventDefault();                              // the paste handler
            break;
          case "deleteContentBackward": case "deleteByCut":
            e.preventDefault();
            if (!maskBlocked(st)) { maskDelete(el, $el, true); }
            break;
          case "deleteContentForward":
            e.preventDefault();
            if (!maskBlocked(st)) { maskDelete(el, $el, false); }
            break;
        }
      });
      $in.on("paste" + NS, function (je) {
        var e = je.originalEvent, text = e && e.clipboardData && e.clipboardData.getData("text/plain");
        je.preventDefault();
        if (text && !maskBlocked(st)) { maskInsert(el, $el, text); }
      });
      // safety net: whatever got through, show the items again
      $in.on("input" + NS, function () {
        var s = selection(st);
        if (st.input.value !== maskText(st)) {
          st.input.value = maskText(st);
          setCursor(st, s.start);
        }
      });
      // sigil's snap-to-editable: a click lands on an editable position
      $in.on("mouseup" + NS, function () {
        if (maskBlocked(st)) { return; }
        var s = selection(st), n = s.start, e = firstEmpty(st);
        if (s.start !== s.end) { return; }
        // not past the first empty position
        if (e >= 0 && n > e) { setCursor(st, e); return; }
        if (editable(st, n)) { return; }
        n = n >= st.items.length ? prevEditable(st, st.items.length) : nextEditable(st, n);
        if (n < 0) { n = prevEditable(st, s.start); }
        setCursor(st, n < 0 ? 0 : n);
      });
      $in.on("focus" + NS, function () {
        $el.addClass("ah-masked-input-focused");
        st.$label.addClass("ah-masked-input-label-float");
        st.input.value = maskDisplay(st);
        st.focusValue = $el.attr("data-ah-value");
        setTimeout(function () {
          if (document.activeElement !== st.input) { return; }
          var e = firstEmpty(st);
          if (e >= 0) { setCursor(st, e); }
        }, 0);
      });
      $in.on("blur" + NS, function () {
        $el.removeClass("ah-masked-input-focused");
        st.$label.toggleClass("ah-masked-input-label-float", maskRaw(st) !== "");
        st.input.value = maskText(st);
        var v = $el.attr("data-ah-value");
        if (st.focusValue !== null && v !== st.focusValue) { $el.trigger("change", [v]); }
        st.focusValue = null;
      });
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = maskState(el);
        maskFill(st.items, v);
        maskShow(el, $el, null, false);
        if (st.focusValue !== null) { st.focusValue = $el.attr("data-ah-value"); }
      },
      getValue: function (el, $el) { return $el.attr("data-ah-value"); },
      getMaskedValue: function (el) { return maskDisplay(maskState(el)); },
      isComplete: function (el) {
        return maskState(el).items.every(function (it) { return it.lit != null || it.ch != null; });
      },
      clear: function (el, $el) {
        var st = maskState(el), old = $el.attr("data-ah-value");
        maskFill(st.items, "");
        maskShow(el, $el, null, false);
        if (old !== "") { $el.trigger("change", [""]); }
      },
      setMask: function (el, $el, mask) {
        var st = maskState(el), raw = maskRaw(st);
        st.items = maskFill(maskParse(mask), raw);
        $el.attr("data-ah-mask", mask);
        maskShow(el, $el, null, false);
      },
      focus: function (el) { maskState(el).input.focus(); }
    }
  });

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

  // ------------------------------------------------------------------
  // range-selector
  // ------------------------------------------------------------------

  var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

  function erlRound(v) { return v < 0 ? -Math.round(-v) : Math.round(v); }

  // The same formats as aihtml_form_entry:format/2.
  function rsFormat(v, f) {
    var d, h, s;
    switch (f.f) {
      case "fixed": s = v.toFixed(f.n); break;
      case "currency":
        d = erlRound(v);
        s = "$" + (d < 0 ? "-" : "") + String(Math.abs(d)).replace(/\B(?=(\d{3})+(?!\d))/g, ",");
        break;
      case "date":
        d = new Date(Math.floor(v));
        s = (d.getUTCMonth() + 1) + "/" + d.getUTCDate() + "/" + d.getUTCFullYear();
        break;
      case "month": s = MONTHS[new Date(Math.floor(v)).getUTCMonth()]; break;
      case "time":
        d = new Date(Math.floor(v));
        h = d.getUTCHours();
        s = ((h % 12) || 12) + ":" + (d.getUTCMinutes() < 10 ? "0" : "") + d.getUTCMinutes() +
          (h >= 12 ? " PM" : " AM");
        break;
      default:
        s = Math.abs(v - erlRound(v)) < 0.001 ? String(erlRound(v)) : v.toFixed(2);
    }
    return (f.p || "") + s + (f.s || "");
  }

  function tidy(v) { return parseFloat(v.toFixed(9)); }

  function rsState(el) { return $.data(el, "ah-rs"); }

  function rsSnap(st, v) {
    v = st.min + Math.round((v - st.min) / st.step) * st.step;
    return tidy(Math.max(st.min, Math.min(st.max, v)));
  }

  function pct(st, v) { return tidy((v - st.min) / (st.max - st.min) * 100); }

  function rsLayout(el, $el) {
    var st = rsState(el), a = pct(st, st.lo), b = pct(st, st.hi);
    st.$slider.css({ left: a + "%", width: tidy(b - a) + "%" });
    st.$shutL.css({ width: a + "%" });
    st.$shutR.css({ left: b + "%", width: tidy(100 - b) + "%" });
    [[st.$mL, st.lo, a], [st.$mR, st.hi, b]].forEach(function (m) {
      var t = rsFormat(m[1], st.format);
      m[0].css("left", m[2] + "%").attr({ "aria-valuenow": String(m[1]), "aria-valuetext": t });
      m[0].children(".ah-range-selector-marker-value").text(t);
    });
    setValue($el, st.lo + "," + st.hi);
  }

  // Set lo / hi (already bounded); input when it changed, change if asked.
  function rsSet(el, $el, lo, hi, change) {
    var st = rsState(el), old = $el.attr("data-ah-value");
    st.lo = lo; st.hi = hi;
    rsLayout(el, $el);
    var v = $el.attr("data-ah-value");
    if (v !== old) { $el.trigger("input", [v]); }
    if (change && v !== st.committed) {
      st.committed = v;
      $el.trigger("change", [v]);
    }
  }

  // Move one end to v, kept min_span away from the other.
  function rsMoveEnd(el, $el, left, v, change) {
    var st = rsState(el);
    v = rsSnap(st, v);
    if (left) {
      rsSet(el, $el, Math.max(st.min, Math.min(v, tidy(st.hi - st.minSpan))), st.hi, change);
    } else {
      rsSet(el, $el, st.lo, Math.min(st.max, Math.max(v, tidy(st.lo + st.minSpan))), change);
    }
  }

  function pointX(e) {
    var o = e.originalEvent, t = o && (o.touches && o.touches[0] || o.changedTouches && o.changedTouches[0]);
    return (t || e).clientX;
  }

  function rsValueAt(st, x) {
    var r = st.$track[0].getBoundingClientRect();
    var p = r.width > 0 ? Math.max(0, Math.min(1, (x - r.left) / r.width)) : 0;
    return st.min + p * (st.max - st.min);
  }

  function rsDisabled($el) { return $el.hasClass("ah-range-selector-disabled"); }

  function rsDrag(el, $el, e, move) {
    var st = rsState(el);
    if (e.type === "mousedown") {
      if (e.button !== 0) { return; }
      e.preventDefault();
    }
    $(document).off(st.ns);
    $(document).on("mousemove" + st.ns + " touchmove" + st.ns, function (me) {
      if (me.type === "mousemove") { me.preventDefault(); }
      move(pointX(me));
    }).on("mouseup" + st.ns + " touchend" + st.ns + " touchcancel" + st.ns, function () {
      $(document).off(st.ns);
      $el.removeClass("ah-range-selector-dragging");
      rsSet(el, $el, st.lo, st.hi, true);
    });
    $el.addClass("ah-range-selector-dragging");
  }

  AH.define("range-selector", {
    init: function (el, $el) {
      var num = function (a) { return parseFloat(el.getAttribute(a)); };
      var v = (el.getAttribute("data-ah-value") || "").split(",");
      var st = {
        ns: docNS(el),
        min: num("data-ah-min"), max: num("data-ah-max"), step: num("data-ah-step") || 1,
        page: num("data-ah-page") || 10, minSpan: num("data-ah-min-span") || 0,
        format: JSON.parse(el.getAttribute("data-ah-format") || "{}"),
        lo: parseFloat(v[0]), hi: parseFloat(v[1]),
        committed: el.getAttribute("data-ah-value"),
        $track: $el.children(".ah-range-selector-track")
      };
      st.$slider = st.$track.children(".ah-range-selector-slider");
      st.$shutL = st.$track.children(".ah-range-selector-shutter-left");
      st.$shutR = st.$track.children(".ah-range-selector-shutter-right");
      st.$mL = st.$track.children(".ah-range-selector-marker-left");
      st.$mR = st.$track.children(".ah-range-selector-marker-right");
      $.data(el, "ah-rs", st);

      st.$track.on("mousedown" + NS + " touchstart" + NS, ".ah-range-selector-marker", function (e) {
        if (rsDisabled($el)) { return; }
        var left = $(this).hasClass("ah-range-selector-marker-left");
        this.focus();
        rsDrag(el, $el, e, function (x) { rsMoveEnd(el, $el, left, rsValueAt(st, x), false); });
      });
      st.$slider.on("mousedown" + NS + " touchstart" + NS, function (e) {
        if (rsDisabled($el)) { return; }
        var span = tidy(st.hi - st.lo), grab = rsValueAt(st, pointX(e)) - st.lo;
        rsDrag(el, $el, e, function (x) {
          var lo = rsSnap(st, Math.max(st.min, Math.min(st.max - span, rsValueAt(st, x) - grab)));
          rsSet(el, $el, lo, Math.min(st.max, tidy(lo + span)), false);
        });
      });
      st.$track.on("keydown" + NS, ".ah-range-selector-marker", function (e) {
        if (rsDisabled($el)) { return; }
        var left = $(this).hasClass("ah-range-selector-marker-left"), cur = left ? st.lo : st.hi, to;
        switch (e.key) {
          case "ArrowRight": case "ArrowUp": to = cur + st.step; break;
          case "ArrowLeft": case "ArrowDown": to = cur - st.step; break;
          case "PageUp": to = cur + st.page; break;
          case "PageDown": to = cur - st.page; break;
          case "Home": to = st.min; break;
          case "End": to = st.max; break;
          default: return;
        }
        e.preventDefault();
        rsMoveEnd(el, $el, left, to, true);
      });
    },
    destroy: function (el) {
      var st = rsState(el);
      if (st) { $(document).off(st.ns); }
    },
    methods: {
      setValue: function (el, $el, v) {
        var st = rsState(el);
        if (typeof v === "string") { v = v.split(","); }
        var lo = rsSnap(st, parseFloat(v[0])), hi = rsSnap(st, parseFloat(v[1]));
        if (lo > hi) { var t = lo; lo = hi; hi = t; }
        st.lo = lo; st.hi = hi;
        rsLayout(el, $el);
        st.committed = $el.attr("data-ah-value");
      },
      getValue: function (el) { var st = rsState(el); return [st.lo, st.hi]; }
    }
  });

  // ------------------------------------------------------------------
  // repeat-button
  // ------------------------------------------------------------------

  function rbState(el) { return $.data(el, "ah-rb"); }

  function rbRelease(el) {
    var st = rbState(el);
    if (!st || !st.active) { return; }
    st.active = false;
    stopRepeat(st);
    $(el).removeClass("ah-btn-pressed");
    // the click the browser sends for this release is not another repetition
    st.swallow = true;
    setTimeout(function () { st.swallow = false; }, 0);
  }

  AH.define("repeat-button", {
    init: function (el, $el) {
      var st = { active: false, swallow: false, timer: null, iv: null };
      $.data(el, "ah-rb", st);
      var delay = parseInt(el.getAttribute("data-ah-delay"), 10);
      var interval = parseInt(el.getAttribute("data-ah-interval"), 10) || 50;
      if (isNaN(delay)) { delay = 300; }
      function press() {
        if (el.disabled || st.active) { return; }
        st.active = true;
        $el.addClass("ah-btn-pressed");
        startRepeat(st, function () {
          if (el.disabled) { rbRelease(el); return; }
          $el.trigger("click");
        }, delay, interval);
      }
      $el.on("mousedown" + NS, function (e) { if (e.button === 0) { press(); } });
      $el.on("touchstart" + NS, function (e) {
        e.preventDefault();                   // no emulated mouse events, no click
        press();
      });
      $el.on("mouseup" + NS + " mouseleave" + NS + " touchend" + NS + " touchcancel" + NS +
             " blur" + NS, function () { rbRelease(el); });
      $el.on("keydown" + NS, function (e) {
        if (e.key !== "Enter" && e.key !== " ") { return; }
        e.preventDefault();
        press();                              // auto-repeated keydowns are ignored
      });
      $el.on("keyup" + NS, function (e) {
        if (e.key === "Enter" || e.key === " ") { e.preventDefault(); rbRelease(el); }
      });
      $el.on("click" + NS, function (e) {
        if (!e.isTrigger && (st.swallow || st.active)) {
          e.preventDefault();
          e.stopImmediatePropagation();
        }
      });
    },
    destroy: function (el) {
      var st = rbState(el);
      if (st) { st.active = false; stopRepeat(st); }
    },
    methods: {
      stop: function (el) { rbRelease(el); }
    }
  });
})(window.jQuery, window.AH);
