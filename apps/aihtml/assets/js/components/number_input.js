/* Behaviour of the number_input component (designs/04-components.md).
 * Ported from sigil: form/number_input. */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var L = AH.lib.input;

  function numOpts($el) {
    var num = function (a) {
      var v = $el.attr(a);
      return v === undefined || v === "" ? null : parseFloat(v);
    };
    return {
      min: num("data-min"),
      max: num("data-max"),
      step: num("data-step") || 1,
      decimals: parseInt($el.attr("data-decimals") || "0", 10),
      allowNull: $el.attr("data-allow-null") !== "false"
    };
  }

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

  var numSeq = 0;

  function numInput($el) { return $el.find("input.ah-numinput-input").first(); }

  // Write a (clamped, formatted) value; returns the value written.
  function writeNum($el, v) {
    var o = numOpts($el), $input = numInput($el);
    if (v === null) {
      v = o.allowNull ? null : clamp(0, o);
    } else {
      v = clamp(v, o);
    }
    $input.val(v === null ? "" : v.toFixed(o.decimals));
    if (v === null) { $input.removeAttr("aria-valuenow"); } else { $input.attr("aria-valuenow", v); }
    L.syncLabel($el, "ah-numinput", v !== null, $input.is(":focus"));
    return v;
  }

  function step($el, dir) {
    var $input = numInput($el), input = $input[0];
    if (!input || input.disabled || input.readOnly) { return; }
    var o = numOpts($el);
    var before = $input.val();
    var cur = parseNum(before);
    var next = parseFloat(((cur === null ? 0 : cur) + o.step * dir).toFixed(o.decimals));
    writeNum($el, next);
    if ($input.val() !== before) {
      $input.trigger("input").trigger("change");
    }
  }

  function stopRepeat(el) {
    var t = $.data(el, "ah-spin");
    if (t) { clearTimeout(t.delay); clearInterval(t.every); }
    $.removeData(el, "ah-spin");
  }

  AH.define("number-input", {
    init: function (el, $el) {
      var $input = numInput($el);
      L.focusShell($el, $input, "ah-numinput");
      $input.on("keydown" + NS, function (e) {
        var k = e.key || "";
        if (e.ctrlKey || e.metaKey || e.altKey) { return; }
        if (k === "ArrowUp" || k === "ArrowDown") {
          e.preventDefault();
          step($el, k === "ArrowUp" ? 1 : -1);
        } else if (k === "PageUp" || k === "PageDown") {
          e.preventDefault();
          step($el, (k === "PageUp" ? 1 : -1) * 10);
        } else if (k === "-") {
          // a minus sign only at the start, once
          if (this.selectionStart !== 0 || this.value.indexOf("-") >= 0) { e.preventDefault(); }
        } else if (k === ".") {
          if (numOpts($el).decimals === 0 || this.value.indexOf(".") >= 0) { e.preventDefault(); }
        } else if (k.length === 1 && !/[0-9]/.test(k)) {
          e.preventDefault();
        }
      });
      // Typing ends in a native change (on blur or Enter): normalise first,
      // this handler runs before the delegated action handlers.
      $input.on("change" + NS, function () {
        writeNum($el, parseNum($input.val()));
      });
      $input.on("wheel" + NS, function (e) {
        if (!$input.is(":focus")) { return; }
        e.preventDefault();
        var dy = (e.originalEvent || e).deltaY;
        step($el, dy < 0 ? 1 : -1);
      });
      // Spin buttons: step, then repeat after 400ms every 75ms (sigil).
      $el.on("mousedown" + NS, ".ah-numinput-spin-up, .ah-numinput-spin-down", function (e) {
        if (e.button !== 0) { return; }
        e.preventDefault();
        var dir = $(this).hasClass("ah-numinput-spin-up") ? 1 : -1;
        stopRepeat(el);
        step($el, dir);
        var t = {};
        t.delay = setTimeout(function () {
          t.every = setInterval(function () { step($el, dir); }, 75);
        }, 400);
        $.data(el, "ah-spin", t);
        if (!$input.is(":focus")) { $input.trigger("focus"); }
      });
      $el.on("mouseleave" + NS, ".ah-numinput-spin", function () { stopRepeat(el); });
      var docNs = NS + "num" + (++numSeq);
      $.data(el, "ah-doc-ns", docNs);
      $(document).on("mouseup" + docNs, function () { stopRepeat(el); });
    },
    destroy: function (el) {
      stopRepeat(el);
      $(document).off("mouseup" + $.data(el, "ah-doc-ns"));
    },
    methods: {
      getValue: function (el, $el) { return parseNum(numInput($el).val()); },
      setValue: function (el, $el, v) {
        var $input = numInput($el), before = $input.val();
        writeNum($el, v === null || v === undefined ? null : parseNum(v));
        if ($input.val() !== before) { $input.trigger("change"); }
      },
      stepUp: function (el, $el) { step($el, 1); },
      stepDown: function (el, $el) { step($el, -1); },
      clear: function (el, $el) {
        var $input = numInput($el), before = $input.val();
        writeNum($el, null);
        if ($input.val() !== before) { $input.trigger("change"); }
      },
      focus: function (el, $el) { numInput($el).trigger("focus"); }
    }
  });
})(window.jQuery, window.AH);
