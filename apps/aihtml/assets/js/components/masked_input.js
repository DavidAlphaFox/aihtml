/* Behaviour of masked_input (designs/04-components.md), after sigil's
 * form/masked_input.
 *
 *   masked-input     only characters that fit the mask are typed, deleted
 *                    or pasted; input on each edit, change on blur
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change".
 *
 * The server renders the whole first state, so init only binds events.
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;

  function setValue($el, v) {
    $el.attr("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  }

  // ------------------------------------------------------------------
  // masked-input
  // ------------------------------------------------------------------
  //
  // The mask is a list of positions {re, ch} (editable, ch null when
  // empty) or {lit} (a literal), parsed as aihtml_masked_input:parse_mask.

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
})(window.jQuery, window.AH);
