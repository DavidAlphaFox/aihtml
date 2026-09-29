/* Behaviour of slider (designs/04-components.md). Ported from sigil
   (sigil.components.form.slider). */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

// Value-bearing contract: data-ah-value + hidden input (change/input
// are fired by the caller).
function writeValue($el, value) {
  $el.attr("data-ah-value", value);
  $el.children("input[type=hidden]").val(value);
}

// ------------------------------------------------------------------
// slider
// ------------------------------------------------------------------
//
// Positions are written as calc() fractions of the track (as the server
// renders them), so nothing needs re-measuring on resize. Keyboard, per
// sigil: Left/Down decrease, Right/Up increase, Home/End; plus
// PageUp/PageDown by ten steps. Buttons step like sigil's; the wheel
// steps while the slider has focus.

var THUMB = 18;

function slConf(el, $el) {
  var c = $.data(el, "ah-slider");
  if (!c) {
    var step = parseFloat($el.attr("data-ah-step")) || 1;
    c = {
      min: parseFloat($el.attr("data-ah-min")) || 0,
      max: parseFloat($el.attr("data-ah-max")),
      step: step,
      decimals: (String(step).split(".")[1] || "").length,
      minRange: parseFloat($el.attr("data-ah-min-range")) || 0,
      vertical: $el.hasClass("ah-slider-vertical"),
      range: $el.hasClass("ah-slider-range-slider")
    };
    if (isNaN(c.max)) { c.max = 100; }
    $.data(el, "ah-slider", c);
  }
  return c;
}

function slValues($el) {
  return String($el.attr("data-ah-value") || "").split(",").map(parseFloat);
}

function slSnap(c, v) {
  var n = Math.round((v - c.min) / c.step);
  var snapped = c.min + n * c.step;
  snapped = Math.max(c.min, Math.min(c.max, snapped));
  return parseFloat(snapped.toFixed(c.decimals));
}

function frac(r) {
  return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + ")";
}

function fracCenter(r) {
  return "calc((100% - " + THUMB + "px) * " + (+r.toFixed(4)) + " + " + THUMB / 2 + "px)";
}

function slRender(el, $el, vals) {
  var c = slConf(el, $el);
  var ratio = function (v) { return (v - c.min) / (c.max - c.min); };
  var $range = $el.find(".ah-slider-range");
  var $end = $el.find(".ah-slider-thumb-end");
  var $start = $el.find(".ah-slider-thumb-start");
  var pos = function ($t, v) {
    if (c.vertical) { $t.css("top", frac(1 - ratio(v))); } else { $t.css("left", frac(ratio(v))); }
  };
  if (c.range) {
    pos($start, vals[0]);
    pos($end, vals[1]);
    if (c.vertical) {
      $range.css({ bottom: fracCenter(ratio(vals[0])), height: frac(ratio(vals[1]) - ratio(vals[0])) });
    } else {
      $range.css({ left: fracCenter(ratio(vals[0])), width: frac(ratio(vals[1]) - ratio(vals[0])) });
    }
    $start.attr({ "aria-valuenow": vals[0], "aria-valuetext": vals[0] });
    $end.attr({ "aria-valuenow": vals[1], "aria-valuetext": vals[1] });
  } else {
    pos($end, vals[0]);
    if (c.vertical) { $range.css({ bottom: 0, height: fracCenter(ratio(vals[0])) }); }
    else { $range.css({ left: 0, width: fracCenter(ratio(vals[0])) }); }
    $el.attr({ "aria-valuenow": vals[0], "aria-valuetext": vals[0] });
  }
  writeValue($el, vals.join(","));
  slTooltip(el, $el);
}

function slTooltip(el, $el) {
  var $tip = $el.children(".ah-slider-tooltip");
  var s = $.data(el, "ah-slider-state") || {};
  if (!$tip.length) { return; }
  var thumb = s.thumb === "start" ? $el.find(".ah-slider-thumb-start")[0]
    : $el.find(".ah-slider-thumb-end")[0];
  var vals = slValues($el);
  $tip.text(s.thumb === "start" ? vals[0] : vals[vals.length - 1]);
  var root = el.getBoundingClientRect();
  var r = thumb.getBoundingClientRect();
  if (slConf(el, $el).vertical) {
    $tip.css("top", (r.top + r.height / 2 - root.top) + "px");
  } else {
    $tip.css("left", (r.left + r.width / 2 - root.left) + "px");
  }
}

// Set one thumb ("start" | "end") to v, keeping the range ordered.
function slSet(el, $el, which, v) {
  var c = slConf(el, $el);
  var vals = slValues($el);
  v = slSnap(c, v);
  if (c.range) {
    if (which === "start") { vals[0] = Math.min(v, vals[1] - c.minRange); }
    else { vals[1] = Math.max(v, vals[0] + c.minRange); }
    vals[0] = Math.max(c.min, vals[0]);
    vals[1] = Math.min(c.max, vals[1]);
  } else {
    vals = [v];
  }
  var old = $el.attr("data-ah-value");
  slRender(el, $el, vals);
  return $el.attr("data-ah-value") !== old;
}

function slFromPointer(el, $el, e) {
  var c = slConf(el, $el);
  var r = $el.find(".ah-slider-track")[0].getBoundingClientRect();
  var ratio = c.vertical
    ? 1 - (e.clientY - r.top - THUMB / 2) / (r.height - THUMB)
    : (e.clientX - r.left - THUMB / 2) / (r.width - THUMB);
  ratio = Math.max(0, Math.min(1, ratio));
  return c.min + ratio * (c.max - c.min);
}

function slDisabled($el) {
  return $el.hasClass("ah-slider-disabled") || $el.attr("aria-disabled") === "true";
}

function slShowTip($el, on) {
  $el.children(".ah-slider-tooltip").toggleClass("ah-slider-tooltip-visible", on);
}

function slStep(el, $el, which, delta) {
  var c = slConf(el, $el);
  var vals = slValues($el);
  var cur = c.range ? (which === "start" ? vals[0] : vals[1]) : vals[0];
  if (slSet(el, $el, which, cur + delta)) {
    $el.trigger("input").trigger("change");
  }
}

AH.define("slider", {
  init: function (el, $el) {
    var c = slConf(el, $el);
    var state = {};
    $.data(el, "ah-slider-state", state);

    $el.on("pointerdown" + NS, ".ah-slider-content", function (e) {
      if (slDisabled($el) || e.button !== 0) { return; }
      e.preventDefault();
      var $thumb = $(e.target).closest(".ah-slider-thumb");
      var v = slFromPointer(el, $el, e);
      var which = "end";
      if (c.range) {
        if ($thumb.length) {
          which = $thumb.hasClass("ah-slider-thumb-start") ? "start" : "end";
        } else {
          var vals = slValues($el);
          which = Math.abs(v - vals[0]) <= Math.abs(v - vals[1]) ? "start" : "end";
        }
      }
      state.dragging = true;
      state.thumb = which;
      state.startValue = $el.attr("data-ah-value");
      var $t = $el.find(".ah-slider-thumb-" + which).addClass("ah-slider-thumb-dragging");
      (c.range ? $t[0] : el).focus({ preventScroll: true });
      slShowTip($el, true);
      try { el.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
      // Pressing the track jumps the nearest thumb there.
      if (!$thumb.length && slSet(el, $el, which, v)) { $el.trigger("input"); }
      slTooltip(el, $el);
    });
    $el.on("pointermove" + NS, function (e) {
      if (!state.dragging) { return; }
      if (slSet(el, $el, state.thumb, slFromPointer(el, $el, e))) { $el.trigger("input"); }
    });
    $el.on("pointerup" + NS + " pointercancel" + NS, function (e) {
      if (!state.dragging) { return; }
      state.dragging = false;
      $el.find(".ah-slider-thumb").removeClass("ah-slider-thumb-dragging");
      try { el.releasePointerCapture(e.pointerId); } catch (err) { /* not captured */ }
      if (!el.contains(document.activeElement)) { slShowTip($el, false); }
      if ($el.attr("data-ah-value") !== state.startValue) { $el.trigger("change"); }
    });

    $el.on("click" + NS, ".ah-slider-button", function () {
      if (slDisabled($el)) { return; }
      var inc = $(this).hasClass("ah-slider-button-next");
      // sigil: in range mode "+" moves the end thumb, "-" the start one
      slStep(el, $el, c.range ? (inc ? "end" : "start") : "end", inc ? c.step : -c.step);
    });

    $el.on("keydown" + NS, function (e) {
      if (slDisabled($el)) { return; }
      var which = "end";
      if (c.range) {
        var $t = $(e.target).closest(".ah-slider-thumb");
        if (!$t.length) { return; }
        which = $t.hasClass("ah-slider-thumb-start") ? "start" : "end";
      }
      state.thumb = which;
      var big = c.step * Math.max(1, Math.round((c.max - c.min) / c.step / 10));
      var delta = { ArrowRight: c.step, ArrowUp: c.step, ArrowLeft: -c.step, ArrowDown: -c.step,
                    PageUp: big, PageDown: -big }[e.key];
      if (delta !== undefined) {
        e.preventDefault();
        slShowTip($el, true);
        slStep(el, $el, which, delta);
      } else if (e.key === "Home" || e.key === "End") {
        e.preventDefault();
        slShowTip($el, true);
        if (slSet(el, $el, which, e.key === "Home" ? c.min : c.max)) {
          $el.trigger("input").trigger("change");
        }
      }
    });

    el.addEventListener("wheel", state.wheel = function (e) {
      if (slDisabled($el) || !el.contains(document.activeElement)) { return; }
      e.preventDefault();
      var which = c.range && $(document.activeElement).hasClass("ah-slider-thumb-start")
        ? "start" : "end";
      slStep(el, $el, which, e.deltaY < 0 ? c.step : -c.step);
    }, { passive: false });

    $el.on("focusin" + NS, function (e) {
      $el.addClass("ah-slider-focused");
      if (c.range) {
        state.thumb = $(e.target).hasClass("ah-slider-thumb-start") ? "start" : "end";
      }
      slTooltip(el, $el);
    });
    $el.on("focusout" + NS, function (e) {
      if (!e.relatedTarget || !el.contains(e.relatedTarget)) {
        $el.removeClass("ah-slider-focused");
        if (!state.dragging) { slShowTip($el, false); }
      }
    });
  },
  destroy: function (el) {
    var s = $.data(el, "ah-slider-state");
    if (s && s.wheel) { el.removeEventListener("wheel", s.wheel); }
    $.removeData(el, "ah-slider");
    $.removeData(el, "ah-slider-state");
  },
  methods: {
    getValue: function (el, $el) { return $el.attr("data-ah-value"); },
    // setValue(v | [lo, hi] | "lo,hi"[, silent])
    setValue: function (el, $el, v, silent) {
      var c = slConf(el, $el);
      var vals = Array.isArray(v) ? v : String(v).split(",");
      vals = vals.map(function (x) { return slSnap(c, parseFloat(x)); });
      if (c.range) { vals = [Math.min(vals[0], vals[1]), Math.max(vals[0], vals[1])]; }
      var old = $el.attr("data-ah-value");
      slRender(el, $el, vals);
      if (!silent && $el.attr("data-ah-value") !== old) { $el.trigger("change"); }
    }
  }
});
