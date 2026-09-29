/* Behaviour of the colorpicker (designs/04-components.md). Ported from
 * sigil: form/colorpicker (+ colorpicker/color, events, render).
 *
 * Value-bearing: data-ah-value and the hidden input follow the value, the
 * root fires `change` on commit and `input` while dragging. Native
 * input/change events of the inner text fields are stopped at the root so
 * they are not taken for the component's own events. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_picker.js";

var NS = AH.NS;
var P = AH.lib.picker;
var uid = P.uid, clamp = P.clamp, round2 = P.round2, disabled = P.disabled,
  fenceNativeEvents = P.fenceNativeEvents, openPopup = P.openPopup,
  closePopup = P.closePopup, stopFloat = P.stopFloat, pointerXY = P.pointerXY, drag = P.drag;

function popupOf($el) { return P.popupOf($el, CP); }
function isOpen($el) { return P.isOpen($el, CP); }

// ------------------------------------------------------------------
// colorpicker (sigil colorpicker, colorpicker/color, events, render)
// ------------------------------------------------------------------

var CP = "ah-colorpicker";

function hsvToRgb(h, s, v) {
  s /= 100; v /= 100;
  var c = v * s;
  var x = c * (1 - Math.abs(((h / 60) % 2) - 1));
  var m = v - c;
  var rgb = h < 60 ? [c, x, 0] : h < 120 ? [x, c, 0] : h < 180 ? [0, c, x]
    : h < 240 ? [0, x, c] : h < 300 ? [x, 0, c] : [c, 0, x];
  return { r: Math.round((rgb[0] + m) * 255), g: Math.round((rgb[1] + m) * 255),
           b: Math.round((rgb[2] + m) * 255) };
}

function rgbToHsv(r, g, b) {
  r /= 255; g /= 255; b /= 255;
  var max = Math.max(r, g, b), min = Math.min(r, g, b), d = max - min;
  var h = d === 0 ? 0 : max === r ? 60 * ((((g - b) / d) % 6 + 6) % 6)
    : max === g ? 60 * ((b - r) / d + 2) : 60 * ((r - g) / d + 4);
  return { h: Math.round(h) % 360, s: Math.round(max === 0 ? 0 : d / max * 100),
           v: Math.round(max * 100) };
}

function hex2(n) { return (n < 16 ? "0" : "") + n.toString(16); }

// "#rgb", "rgb", "#rrggbb", with alpha also 4 and 8 digits -> {r,g,b,a}
function parseHex(s, alpha) {
  var h = String(s || "").trim().replace(/^#/, "");
  if (!/^[0-9a-f]+$/i.test(h)) { return null; }
  if (h.length === 3 || (alpha && h.length === 4)) {
    h = h.replace(/./g, "$&$&");
  }
  if (h.length !== 6 && !(alpha && h.length === 8)) { return null; }
  return { r: parseInt(h.slice(0, 2), 16), g: parseInt(h.slice(2, 4), 16),
           b: parseInt(h.slice(4, 6), 16), a: h.length === 8 ? parseInt(h.slice(6, 8), 16) : 255 };
}

function cpState(el) { return $.data(el, "ah-cp"); }

// The exact RGB a colour was loaded with (hex, RGB inputs, swatch) is
// kept until the HSV controls change it, so rounding through HSV does
// not alter a typed colour.
function cpRgb(st) {
  var e = st.exact;
  if (e && e.h === st.h && e.s === st.s && e.v === st.v) { return e.rgb; }
  return hsvToRgb(st.h, st.s, st.v);
}

function cpHex(st) {
  var c = cpRgb(st);
  var hex = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
  return st.alpha && st.a < 255 ? hex + hex2(st.a) : hex;
}

function cpRgba(st) {
  var c = cpRgb(st);
  return "rgba(" + c.r + "," + c.g + "," + c.b + "," + round2(st.a / 255) + ")";
}

function cpLoad(st, c) {
  var hsv = rgbToHsv(c.r, c.g, c.b);
  // Keep the hue when the colour is grey or black, so the area does
  // not jump back to red.
  if (hsv.s === 0 || hsv.v === 0) { hsv.h = st.h; }
  if (hsv.v === 0) { hsv.s = st.s; }
  st.h = hsv.h; st.s = hsv.s; st.v = hsv.v; st.a = c.a;
  st.exact = { h: st.h, s: st.s, v: st.v, rgb: { r: c.r, g: c.g, b: c.b } };
}

// sigil 同步全部UI!: area colour, pointers, preview, inputs; plus the
// alpha bar, swatches, ARIA and the popup trigger.
function cpSync(el, skip) {
  var st = cpState(el);
  var $el = $(el);
  var c = cpRgb(st);
  var bright = 0.299 * c.r + 0.587 * c.g + 0.114 * c.b > 150;
  var hex6 = "#" + hex2(c.r) + hex2(c.g) + hex2(c.b);
  var $map = $el.find("." + CP + "-map");
  $map.css("background-color", "hsl(" + st.h + ", 100%, 50%)")
    .attr({ "aria-valuenow": st.s,
            "aria-valuetext": "Saturation " + st.s + "%, brightness " + st.v + "%" });
  $map.find("." + CP + "-map-pointer").css({ left: st.s + "%", top: (100 - st.v) + "%" })
    .toggleClass(CP + "-map-pointer-dark", bright)
    .toggleClass(CP + "-map-pointer-light", !bright);
  var $hue = $el.find("." + CP + "-bar").not("." + CP + "-alpha");
  $hue.attr("aria-valuenow", st.h).find("." + CP + "-bar-pointer").css("top", (st.h / 360 * 100) + "%");
  var pct = Math.round(st.a / 255 * 100);
  var $alpha = $el.find("." + CP + "-alpha");
  $alpha.css("--ah-cp-rgb", hex6).attr({ "aria-valuenow": pct, "aria-valuetext": pct + "%" })
    .find("." + CP + "-bar-pointer").css("top", (100 - pct) + "%");
  $el.find("." + CP + "-preview").css("background-color", cpRgba(st));
  if (skip !== "hex") {
    $el.find("." + CP + "-hex-input").val(cpHex(st).slice(1));
  }
  if (skip !== "rgb") {
    $el.find("." + CP + "-r-input").val(c.r);
    $el.find("." + CP + "-g-input").val(c.g);
    $el.find("." + CP + "-b-input").val(c.b);
    $el.find("." + CP + "-a-input").val(pct);
  }
}

// The value-bearing side: data-ah-value, hidden input, trigger, swatches.
function cpSetValue(el, value) {
  var $el = $(el);
  var st = cpState(el);
  el.setAttribute("data-ah-value", value);
  $el.children("input[type=hidden]").val(value);
  var $trigger = $el.children("." + CP + "-trigger");
  $trigger.find("." + CP + "-trigger-swatch")
    .toggleClass(CP + "-trigger-empty", value === "")
    .css("--ah-cp-swatch", value === "" ? "" : cpRgba(st));
  $trigger.find("." + CP + "-trigger-text")
    .text(value === "" ? ($trigger.attr("data-placeholder") || "") : value);
  $el.find("." + CP + "-swatch").each(function () {
    this.setAttribute("aria-pressed", String(this.getAttribute("data-color") === value));
  });
}

// type: "input" (live) or "change" (commit). change fires only when the
// value differs from the last committed one.
function cpEmit(el, type, skip) {
  var st = cpState(el);
  cpSync(el, skip);
  var value = cpHex(st);
  var before = el.getAttribute("data-ah-value");
  cpSetValue(el, value);
  if (type === "input") {
    if (value !== before) { $(el).trigger("input", [{ value: value }]); }
  } else {
    cpCommit(el, value);
  }
}

function cpCommit(el, value) {
  var st = cpState(el);
  if (value !== st.committed) {
    st.committed = value;
    $(el).trigger("change", [{ value: value }]);
  }
}

function cpClear(el) {
  cpSetValue(el, "");
  cpCommit(el, "");
}

AH.define("colorpicker", {
  init: function (el, $el) {
    el.setAttribute("data-ah-uid", P.nextId());
    var value = el.getAttribute("data-ah-value") || "";
    var alpha = el.getAttribute("data-alpha") === "true";
    var st = { h: 0, s: 100, v: 100, a: 255, alpha: alpha, committed: value };
    $.data(el, "ah-cp", st);
    var start = parseHex(value, alpha);
    if (start) { cpLoad(st, start); }
    var $trigger = $el.children("." + CP + "-trigger");
    var $popup = popupOf($el);
    var popup = $popup.length > 0;
    if (popup) {
      $popup.attr("id", $popup.attr("id") || uid("ah-cp-popup-"));
      $trigger.attr("aria-controls", $popup.attr("id"));
    }
    fenceNativeEvents($el);

    function open() {
      openPopup(el, $el, CP, $trigger, function () { cpSync(el); });
      $el.find("." + CP + "-map").trigger("focus");
    }
    function close(refocus) { closePopup(el, $el, CP, $trigger, refocus); }
    $.data(el, "ah-cp-open", open);
    $.data(el, "ah-cp-close", close);

    $trigger.on("click" + NS, function (e) {
      e.preventDefault();
      if (isOpen($el)) { close(true); } else { open(); }
    });
    $trigger.on("keydown" + NS, function (e) {
      if (e.key === "ArrowDown") { e.preventDefault(); open(); }
    });
    $el.on("keydown" + NS, function (e) {
      if (e.key === "Escape" && isOpen($el)) {
        e.preventDefault();
        e.stopPropagation();
        close(true);
      }
    });

    // Saturation/value area (sigil 处理面板拖拽).
    var $map = $el.find("." + CP + "-map");
    function fromMap(e) {
      var r = $map[0].getBoundingClientRect();
      var p = pointerXY(e);
      st.s = Math.round(clamp((p.x - r.left) / r.width, 0, 1) * 100);
      st.v = Math.round((1 - clamp((p.y - r.top) / r.height, 0, 1)) * 100);
      cpEmit(el, "input");
    }
    function endDrag() { cpEmit(el, "change"); }
    drag($map, el, fromMap, fromMap, endDrag);

    // Hue bar (sigil 处理色相拖拽) and alpha bar.
    var $hue = $el.find("." + CP + "-bar").not("." + CP + "-alpha");
    function fromHue(e) {
      var r = $hue[0].getBoundingClientRect();
      st.h = Math.round(clamp((pointerXY(e).y - r.top) / r.height, 0, 1) * 360) % 360;
      cpEmit(el, "input");
    }
    drag($hue, el, fromHue, fromHue, endDrag);
    var $alpha = $el.find("." + CP + "-alpha");
    function fromAlpha(e) {
      var r = $alpha[0].getBoundingClientRect();
      st.a = Math.round((1 - clamp((pointerXY(e).y - r.top) / r.height, 0, 1)) * 255);
      cpEmit(el, "input");
    }
    if ($alpha.length) { drag($alpha, el, fromAlpha, fromAlpha, endDrag); }

    // Keyboard: arrows move by 1, Shift by 10; Home / End.
    function keys($t, apply) {
      $t.on("keydown" + NS, function (e) {
        if (disabled(el)) { return; }
        var n = e.shiftKey ? 10 : 1;
        if (apply(e.key, n) !== false) {
          e.preventDefault();
          cpEmit(el, "input");
          cpEmit(el, "change");
        }
      });
    }
    keys($map, function (key, n) {
      switch (key) {
        case "ArrowLeft": st.s = clamp(st.s - n, 0, 100); break;
        case "ArrowRight": st.s = clamp(st.s + n, 0, 100); break;
        case "ArrowUp": st.v = clamp(st.v + n, 0, 100); break;
        case "ArrowDown": st.v = clamp(st.v - n, 0, 100); break;
        case "Home": st.s = 0; break;
        case "End": st.s = 100; break;
        default: return false;
      }
    });
    // The hue grows downwards on the bar, so Down increases it.
    keys($hue, function (key, n) {
      switch (key) {
        case "ArrowDown": case "ArrowRight": st.h = (st.h + n) % 360; break;
        case "ArrowUp": case "ArrowLeft": st.h = (st.h - n + 360) % 360; break;
        case "Home": st.h = 0; break;
        case "End": st.h = 359; break;
        default: return false;
      }
    });
    keys($alpha, function (key, n) {
      var step = Math.round(n * 2.55);
      switch (key) {
        case "ArrowUp": case "ArrowRight": st.a = clamp(st.a + step, 0, 255); break;
        case "ArrowDown": case "ArrowLeft": st.a = clamp(st.a - step, 0, 255); break;
        case "Home": st.a = 0; break;
        case "End": st.a = 255; break;
        default: return false;
      }
    });

    // Hex input (sigil 处理hex输入): live while valid, commit on change.
    var $hexIn = $el.find("." + CP + "-hex-input");
    $hexIn.on("input" + NS, function () {
      var c = parseHex($hexIn.val(), alpha);
      var len = String($hexIn.val()).trim().replace(/^#/, "").length;
      if (c && len >= 6) {
        cpLoad(st, c);
        cpEmit(el, "input", "hex");
      }
    });
    $hexIn.on("change" + NS, function () {
      var c = parseHex($hexIn.val(), alpha);
      if (c) { cpLoad(st, c); }
      cpEmit(el, "change");
    });
    $hexIn.on("keydown" + NS, function (e) {
      if (e.key === "Enter") { e.preventDefault(); $hexIn.trigger("change"); }
    });

    // RGB(A) inputs (sigil 处理rgb输入).
    var $rgbIn = $el.find("." + CP + "-r-input, ." + CP + "-g-input, ." + CP + "-b-input, ." + CP + "-a-input");
    function fromRgb() {
      var n = function (cls, max) {
        var v = parseInt($el.find("." + CP + "-" + cls + "-input").val(), 10);
        return isNaN(v) ? null : clamp(v, 0, max);
      };
      var r = n("r", 255), g = n("g", 255), b = n("b", 255);
      if (r === null || g === null || b === null) { return false; }
      var a = alpha ? n("a", 100) : 100;
      cpLoad(st, { r: r, g: g, b: b, a: a === null ? st.a : Math.round(a * 2.55) });
      return true;
    }
    $rgbIn.on("input" + NS, function () {
      if (fromRgb()) { cpEmit(el, "input", "rgb"); }
    });
    $rgbIn.on("change" + NS, function () {
      fromRgb();
      cpEmit(el, "change");
    });

    // Swatches and the clear link (sigil's transparent link).
    $el.on("click" + NS, "." + CP + "-swatch", function (e) {
      e.preventDefault();
      var c = parseHex(this.getAttribute("data-color"), alpha);
      if (!c || disabled(el)) { return; }
      cpLoad(st, c);
      cpEmit(el, "input");
      cpEmit(el, "change");
    });
    $el.on("click" + NS, "." + CP + "-transparent a", function (e) {
      e.preventDefault();
      if (disabled(el)) { return; }
      cpClear(el);
      close(true);
    });
    cpSync(el);
  },
  destroy: function (el) { stopFloat(el); },
  methods: {
    getValue: function (el) { return el.getAttribute("data-ah-value") || ""; },
    // Set the value without firing events (server driven); "" clears.
    setValue: function (el, $el, v) {
      var st = cpState(el);
      var c = parseHex(v == null ? "" : String(v), st.alpha);
      if (!c) {
        st.committed = "";
        cpSetValue(el, "");
        return;
      }
      cpLoad(st, c);
      cpSync(el);
      st.committed = cpHex(st);
      cpSetValue(el, st.committed);
    },
    clear: function (el) { cpClear(el); },
    open: function (el) { $.data(el, "ah-cp-open")(); },
    close: function (el) { $.data(el, "ah-cp-close")(false); }
  }
});
