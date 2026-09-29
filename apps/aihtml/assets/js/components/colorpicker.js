/* Behaviour of the colorpicker (designs/04-components.md). Ported from
 * sigil: form/colorpicker (+ colorpicker/color, events, render).
 *
 * Value-bearing: data-ah-value and the hidden input follow the value, the
 * root fires `change` on commit and `input` while dragging (detail of
 * both: {value}). Native input/change events of the inner text fields are
 * stopped at the root so they are not taken for the component's own
 * events. */
import AH from "../core.js";
import "./_lib_picker.js";

var P = AH.lib.picker;
var uid = P.uid, clamp = P.clamp, round2 = P.round2, disabled = P.disabled,
  pointerXY = P.pointerXY;

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
function cpSync(c, skip) {
  var st = c.st, el = c.element;
  var rgb = cpRgb(st);
  var bright = 0.299 * rgb.r + 0.587 * rgb.g + 0.114 * rgb.b > 150;
  var hex6 = "#" + hex2(rgb.r) + hex2(rgb.g) + hex2(rgb.b);
  var all = function (sel, f) { el.querySelectorAll(sel).forEach(f); };
  all("." + CP + "-map", function (map) {
    map.style.backgroundColor = "hsl(" + st.h + ", 100%, 50%)";
    map.setAttribute("aria-valuenow", st.s);
    map.setAttribute("aria-valuetext", "Saturation " + st.s + "%, brightness " + st.v + "%");
    map.querySelectorAll("." + CP + "-map-pointer").forEach(function (p) {
      p.style.left = st.s + "%";
      p.style.top = (100 - st.v) + "%";
      p.classList.toggle(CP + "-map-pointer-dark", bright);
      p.classList.toggle(CP + "-map-pointer-light", !bright);
    });
  });
  all("." + CP + "-bar:not(." + CP + "-alpha)", function (hue) {
    hue.setAttribute("aria-valuenow", st.h);
    hue.querySelectorAll("." + CP + "-bar-pointer").forEach(function (p) { p.style.top = (st.h / 360 * 100) + "%"; });
  });
  var pct = Math.round(st.a / 255 * 100);
  all("." + CP + "-alpha", function (a) {
    a.style.setProperty("--ah-cp-rgb", hex6);
    a.setAttribute("aria-valuenow", pct);
    a.setAttribute("aria-valuetext", pct + "%");
    a.querySelectorAll("." + CP + "-bar-pointer").forEach(function (p) { p.style.top = (100 - pct) + "%"; });
  });
  all("." + CP + "-preview", function (p) { p.style.backgroundColor = cpRgba(st); });
  if (skip !== "hex") {
    all("." + CP + "-hex-input", function (i) { i.value = cpHex(st).slice(1); });
  }
  if (skip !== "rgb") {
    all("." + CP + "-r-input", function (i) { i.value = rgb.r; });
    all("." + CP + "-g-input", function (i) { i.value = rgb.g; });
    all("." + CP + "-b-input", function (i) { i.value = rgb.b; });
    all("." + CP + "-a-input", function (i) { i.value = pct; });
  }
}

// The value-bearing side: data-ah-value, hidden input, trigger, swatches.
function cpSetValue(c, value) {
  var st = c.st, el = c.element;
  el.setAttribute("data-ah-value", value);
  var hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = value; }
  var trigger = el.querySelector(":scope > ." + CP + "-trigger");
  if (trigger) {
    trigger.querySelectorAll("." + CP + "-trigger-swatch").forEach(function (s) {
      s.classList.toggle(CP + "-trigger-empty", value === "");
      if (value === "") { s.style.removeProperty("--ah-cp-swatch"); } else { s.style.setProperty("--ah-cp-swatch", cpRgba(st)); }
    });
    trigger.querySelectorAll("." + CP + "-trigger-text").forEach(function (t) {
      t.textContent = value === "" ? (trigger.getAttribute("data-placeholder") || "") : value;
    });
  }
  el.querySelectorAll("." + CP + "-swatch").forEach(function (s) {
    s.setAttribute("aria-pressed", String(s.getAttribute("data-color") === value));
  });
}

// type: "input" (live) or "change" (commit). change fires only when the
// value differs from the last committed one.
function cpEmit(c, type, skip) {
  var st = c.st;
  cpSync(c, skip);
  var value = cpHex(st);
  var before = c.element.getAttribute("data-ah-value");
  cpSetValue(c, value);
  if (type === "input") {
    if (value !== before) { c.fire("input", { value: value }); }
  } else {
    cpCommit(c, value);
  }
}

function cpCommit(c, value) {
  var st = c.st;
  if (value !== st.committed) {
    st.committed = value;
    c.fire("change", { value: value });
  }
}

function cpClear(c) {
  cpSetValue(c, "");
  cpCommit(c, "");
}

AH.register("colorpicker", class extends AH.Controller {
  setup() {
    var c = this, el = this.element;
    var q = function (sel) { return el.querySelector(sel); };
    var value = el.getAttribute("data-ah-value") || "";
    var alpha = el.getAttribute("data-alpha") === "true";
    var st = this.st = { h: 0, s: 100, v: 100, a: 255, alpha: alpha, committed: value };
    var start = parseHex(value, alpha);
    if (start) { cpLoad(st, start); }
    var trigger = q(":scope > ." + CP + "-trigger");
    var popupEl = P.popupOf(el, CP);
    if (popupEl) {
      popupEl.id = popupEl.id || uid("ah-cp-popup-");
      if (trigger) { trigger.setAttribute("aria-controls", popupEl.id); }
    }
    P.fenceNativeEvents(this);
    var isOpen = function () { return P.isOpen(el, CP); };
    var map = q("." + CP + "-map");

    var open = this._open = function () {
      P.openPopup(c, CP, trigger, function () { cpSync(c); });
      if (map) { map.focus(); }
    };
    var close = this._close = function (refocus) { P.closePopup(c, CP, trigger, refocus); };

    if (trigger) {
      this.listen(trigger, "click", function (e) {
        e.preventDefault();
        if (isOpen()) { close(true); } else { open(); }
      });
      this.listen(trigger, "keydown", function (e) {
        if (e.key === "ArrowDown") { e.preventDefault(); open(); }
      });
    }
    this.listen(el, "keydown", function (e) {
      if (e.key === "Escape" && isOpen()) {
        e.preventDefault();
        e.stopPropagation();
        close(true);
      }
    });

    // Saturation/value area (sigil 处理面板拖拽).
    function fromMap(e) {
      var r = map.getBoundingClientRect();
      var p = pointerXY(e);
      st.s = Math.round(clamp((p.x - r.left) / r.width, 0, 1) * 100);
      st.v = Math.round((1 - clamp((p.y - r.top) / r.height, 0, 1)) * 100);
      cpEmit(c, "input");
    }
    function endDrag() { cpEmit(c, "change"); }
    P.drag(this, map, fromMap, fromMap, endDrag);

    // Hue bar (sigil 处理色相拖拽) and alpha bar.
    var hue = q("." + CP + "-bar:not(." + CP + "-alpha)");
    function fromHue(e) {
      var r = hue.getBoundingClientRect();
      st.h = Math.round(clamp((pointerXY(e).y - r.top) / r.height, 0, 1) * 360) % 360;
      cpEmit(c, "input");
    }
    P.drag(this, hue, fromHue, fromHue, endDrag);
    var alphaBar = q("." + CP + "-alpha");
    function fromAlpha(e) {
      var r = alphaBar.getBoundingClientRect();
      st.a = Math.round((1 - clamp((pointerXY(e).y - r.top) / r.height, 0, 1)) * 255);
      cpEmit(c, "input");
    }
    P.drag(this, alphaBar, fromAlpha, fromAlpha, endDrag);

    // Keyboard: arrows move by 1, Shift by 10; Home / End.
    function keys(t, apply) {
      if (!t) { return; }
      c.listen(t, "keydown", function (e) {
        if (disabled(el)) { return; }
        var n = e.shiftKey ? 10 : 1;
        if (apply(e.key, n) !== false) {
          e.preventDefault();
          cpEmit(c, "input");
          cpEmit(c, "change");
        }
      });
    }
    keys(map, function (key, n) {
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
    keys(hue, function (key, n) {
      switch (key) {
        case "ArrowDown": case "ArrowRight": st.h = (st.h + n) % 360; break;
        case "ArrowUp": case "ArrowLeft": st.h = (st.h - n + 360) % 360; break;
        case "Home": st.h = 0; break;
        case "End": st.h = 359; break;
        default: return false;
      }
    });
    keys(alphaBar, function (key, n) {
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
    var hexIn = q("." + CP + "-hex-input");
    if (hexIn) {
      this.listen(hexIn, "input", function () {
        var col = parseHex(hexIn.value, alpha);
        var len = String(hexIn.value).trim().replace(/^#/, "").length;
        if (col && len >= 6) {
          cpLoad(st, col);
          cpEmit(c, "input", "hex");
        }
      });
      this.listen(hexIn, "change", function () {
        var col = parseHex(hexIn.value, alpha);
        if (col) { cpLoad(st, col); }
        cpEmit(c, "change");
      });
      this.listen(hexIn, "keydown", function (e) {
        if (e.key === "Enter") { e.preventDefault(); hexIn.dispatchEvent(new Event("change", { bubbles: true })); }
      });
    }

    // RGB(A) inputs (sigil 处理rgb输入).
    function fromRgb() {
      var n = function (cls, max) {
        var i = q("." + CP + "-" + cls + "-input");
        var v = parseInt(i ? i.value : "", 10);
        return isNaN(v) ? null : clamp(v, 0, max);
      };
      var r = n("r", 255), g = n("g", 255), b = n("b", 255);
      if (r === null || g === null || b === null) { return false; }
      var a = alpha ? n("a", 100) : 100;
      cpLoad(st, { r: r, g: g, b: b, a: a === null ? st.a : Math.round(a * 2.55) });
      return true;
    }
    el.querySelectorAll("." + CP + "-r-input, ." + CP + "-g-input, ." + CP + "-b-input, ." + CP + "-a-input")
      .forEach(function (i) {
        c.listen(i, "input", function () {
          if (fromRgb()) { cpEmit(c, "input", "rgb"); }
        });
        c.listen(i, "change", function () {
          fromRgb();
          cpEmit(c, "change");
        });
      });

    // Swatches and the clear link (sigil's transparent link).
    this.delegate("click", "." + CP + "-swatch", function (e, sw) {
      e.preventDefault();
      var col = parseHex(sw.getAttribute("data-color"), alpha);
      if (!col || disabled(el)) { return; }
      cpLoad(st, col);
      cpEmit(c, "input");
      cpEmit(c, "change");
    });
    this.delegate("click", "." + CP + "-transparent a", function (e) {
      e.preventDefault();
      if (disabled(el)) { return; }
      cpClear(c);
      close(true);
    });
    cpSync(this);
  }

  teardown() { P.stopFloat(this); }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return this.element.getAttribute("data-ah-value") || ""; }
  // Set the value without firing events (server driven); "" clears.
  setValue(v) {
    var st = this.st;
    var col = parseHex(v == null ? "" : String(v), st.alpha);
    if (!col) {
      st.committed = "";
      cpSetValue(this, "");
      return;
    }
    cpLoad(st, col);
    cpSync(this);
    st.committed = cpHex(st);
    cpSetValue(this, st.committed);
  }
  clear() { cpClear(this); }
  open() { this._open(); }
  close() { this._close(false); }
});
