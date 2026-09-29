/* Behaviour of the scrollbar component (designs/04-components.md),
 * ported from sigil's scrollbar (cljs + jQuery). A standalone bar keeps
 * its value in data-ah-value on the root, mirrors it into a hidden input
 * when there is one, and fires "change" on the root when the user changes
 * it; methods called by the server update the value without firing
 * "change". Drags use pointer capture, so nothing is bound on document.
 */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_scroll.js";

var NS = AH.NS;
var setValue = AH.lib.scroll.setValue, num = AH.lib.scroll.num, capture = AH.lib.scroll.capture;
var seq = 0;

// ------------------------------------------------------------------
// Scrollbar (sigil layout/scrollbar, and the bars of sigil's panel)
// ------------------------------------------------------------------
//
// One bar is .ah-scrollbar with five parts: up button, track before the
// thumb, thumb, track after it, down button. sbLayout sizes them from a
// value in [min, max] exactly as sigil's arrange! does.

var SB = "ah-scrollbar";
var SB_BTN = 14;

function sbParts(bar) {
  var $b = $(bar);
  return {
    up: $b.children("." + SB + "-btn-up")[0],
    tu: $b.children("." + SB + "-track-up")[0],
    thumb: $b.children("." + SB + "-thumb")[0],
    td: $b.children("." + SB + "-track-down")[0],
    down: $b.children("." + SB + "-btn-down")[0]
  };
}

function sbVertical(bar) { return $(bar).hasClass(SB + "-vertical"); }

// Geometry of a bar for a value: track length, thumb size and position.
function sbGeom(bar, o) {
  var vert = sbVertical(bar);
  var total = vert ? bar.clientHeight : bar.clientWidth;
  var track = Math.max(0, total - (o.buttons ? 2 * SB_BTN : 0));
  var range = o.max - o.min;
  var ts = range <= 0 ? track : Math.max(o.thumbMin, track * track / (track + range));
  ts = Math.min(ts, track);
  var tp = range <= 0 ? 0 : (o.value - o.min) / range * (track - ts);
  return { vert: vert, track: track, ts: ts, tp: tp };
}

function sbLayout(bar, o) {
  var g = sbGeom(bar, o), p = sbParts(bar), dim = g.vert ? "height" : "width";
  p.up.style.display = p.down.style.display = o.buttons ? "" : "none";
  p.tu.style[dim] = g.tp + "px";
  p.thumb.style[dim] = g.ts + "px";
  p.td.style[dim] = Math.max(0, g.track - g.ts - g.tp) + "px";
  return g;
}

function sbPosToValue(bar, o, pos) {
  var g = sbGeom(bar, o), free = g.track - g.ts;
  return free <= 0 ? o.min : o.min + Math.max(0, Math.min(free, pos)) / free * (o.max - o.min);
}

// Repeat fn while a button is held: once now, again after 300ms, then
// every 50ms (sigil util/start-repeat-timer!).
function sbRepeat(node, e, fn) {
  fn();
  var iv = null;
  var t = setTimeout(function () { iv = setInterval(fn, 50); }, 300);
  capture(node, e);
  $(node).on("pointerup.ahrep pointercancel.ahrep lostpointercapture.ahrep", function () {
    clearTimeout(t);
    clearInterval(iv);
    $(node).off(".ahrep");
  });
}

// Wire one bar. api: {opts(), set(value, phase), start()} where phase is
// "input" while dragging, "change" otherwise and "end" when a drag ends.
function sbBind(bar, api) {
  var p = sbParts(bar);
  $(p.up).add(p.down).on("pointerdown" + NS, function (e) {
    if (e.button !== undefined && e.button !== 0) { return; }
    e.preventDefault();
    var dir = this === p.up ? -1 : 1;
    sbRepeat(this, e, function () { var o = api.opts(); api.set(o.value + dir * o.step, "change"); });
  });
  $(p.tu).add(p.td).on("pointerdown" + NS, function (e) {
    if (e.button !== undefined && e.button !== 0) { return; }
    e.preventDefault();
    var o = api.opts();
    api.set(o.value + (this === p.tu ? -1 : 1) * o.large, "change");
  });
  var drag = null;
  $(p.thumb).on("pointerdown" + NS, function (e) {
    if (e.button !== undefined && e.button !== 0) { return; }
    e.preventDefault();
    var o = api.opts(), g = sbGeom(bar, o);
    drag = { start: g.vert ? e.clientY : e.clientX, pos: g.tp, vert: g.vert, value: o.value };
    capture(this, e);
    $(this).addClass(SB + "-thumb-pressed");
    if (api.start) { api.start(); }
  });
  $(p.thumb).on("pointermove" + NS, function (e) {
    if (!drag) { return; }
    var cur = drag.vert ? e.clientY : e.clientX;
    api.set(sbPosToValue(bar, api.opts(), drag.pos + cur - drag.start), "input");
  });
  $(p.thumb).on("pointerup" + NS + " pointercancel" + NS, function () {
    if (!drag) { return; }
    var before = drag.value;
    drag = null;
    $(this).removeClass(SB + "-thumb-pressed");
    api.set(api.opts().value, "end", before);
  });
}

function sbObserve(el, targets, fn) {
  var st = $.data(el, "ahScrollbar");
  if (window.ResizeObserver) {
    st.ro = new ResizeObserver(function () { fn(); });
    targets.forEach(function (t) { if (t) { st.ro.observe(t); } });
  } else {
    st.ns = ".ahsb" + (++seq);
    $(window).on("resize" + st.ns, fn);
  }
}

// --- standalone bar ---------------------------------------------------

function sbOpts(el) {
  return {
    min: num(el, "data-min", 0), max: num(el, "data-max", 1000),
    value: num(el, "data-ah-value", 0),
    step: num(el, "data-step", 10), large: num(el, "data-large-step", 50),
    thumbMin: num(el, "data-thumb-min", 10),
    buttons: el.getAttribute("data-buttons") !== "false"
  };
}

function sbIntegral(o) {
  return o.min % 1 === 0 && o.max % 1 === 0 && o.step % 1 === 0 && o.large % 1 === 0;
}

function sbBar($el) { return $el.children("." + SB)[0]; }

function sbSet(el, $el, v, event) {
  var o = sbOpts(el);
  v = Math.max(o.min, Math.min(o.max, +v || 0));
  if (sbIntegral(o)) { v = Math.round(v); }
  var changed = v !== o.value;
  if (changed) {
    setValue(el, $el, String(v));
    el.setAttribute("aria-valuenow", String(v));
  }
  o.value = v;
  sbLayout(sbBar($el), o);
  if (changed && event) { $el.trigger(event); }
  return changed;
}

function sbStandalone(el, $el) {
  var bar = sbBar($el);
  var disabled = function () { return $el.hasClass(SB + "-disabled"); };
  sbBind(bar, {
    opts: function () { return sbOpts(el); },
    set: function (v, phase, before) {
      if (disabled()) { return; }
      if (phase === "end") {
        if (String(before) !== el.getAttribute("data-ah-value")) { $el.trigger("change"); }
        return;
      }
      sbSet(el, $el, v, phase);
    },
    start: function () { el.focus({ preventScroll: true }); }
  });
  $el.on("keydown" + NS, function (e) {
    if (e.target !== el || disabled()) { return; }
    var o = sbOpts(el), vert = sbVertical(bar), to;
    switch (e.key) {
      case "ArrowLeft": if (vert) { return; } to = o.value - o.step; break;
      case "ArrowRight": if (vert) { return; } to = o.value + o.step; break;
      case "ArrowUp": if (!vert) { return; } to = o.value - o.step; break;
      case "ArrowDown": if (!vert) { return; } to = o.value + o.step; break;
      case "PageUp": to = o.value - o.large; break;
      case "PageDown": to = o.value + o.large; break;
      case "Home": to = o.min; break;
      case "End": to = o.max; break;
      default: return;
    }
    e.preventDefault();
    sbSet(el, $el, to, "change");
  });
  var relayout = function () { sbLayout(bar, sbOpts(el)); };
  sbObserve(el, [el], relayout);
  relayout();
}

// --- scroll area --------------------------------------------------------

function saParts($el) {
  return {
    vp: $el.children("." + SB + "-viewport")[0],
    v: $el.children("." + SB + "-vertical")[0],
    h: $el.children("." + SB + "-horizontal")[0]
  };
}

function saOpts(el, vp, vert) {
  var max = vert ? vp.scrollHeight - vp.clientHeight : vp.scrollWidth - vp.clientWidth;
  var page = vert ? vp.clientHeight : vp.clientWidth;
  return {
    min: 0, max: Math.max(0, max),
    value: vert ? vp.scrollTop : vp.scrollLeft,
    step: num(el, "data-step", 10) * 3, large: Math.max(10, page * 0.9),
    thumbMin: num(el, "data-thumb-min", 10),
    buttons: el.getAttribute("data-buttons") !== "false"
  };
}

// Which bars the content needs: showing one narrows the viewport, which
// may make the other one necessary, so settle it in two passes.
function saLayout(el, $el) {
  var p = saParts($el), vp = p.vp;
  var needV = false, needH = false;
  for (var i = 0; i < 2; i++) {
    $el.toggleClass(SB + "-area-v", needV).toggleClass(SB + "-area-h", needH);
    needV = vp.scrollHeight > vp.clientHeight + 1;
    needH = vp.scrollWidth > vp.clientWidth + 1;
  }
  $el.toggleClass(SB + "-area-v", needV).toggleClass(SB + "-area-h", needH);
  if (needV) { sbLayout(p.v, saOpts(el, vp, true)); }
  if (needH) { sbLayout(p.h, saOpts(el, vp, false)); }
}

function saSync(el, $el) {
  var p = saParts($el);
  if ($el.hasClass(SB + "-area-v")) { sbLayout(p.v, saOpts(el, p.vp, true)); }
  if ($el.hasClass(SB + "-area-h")) { sbLayout(p.h, saOpts(el, p.vp, false)); }
}

function saArea(el, $el) {
  var p = saParts($el);
  [[p.v, true], [p.h, false]].forEach(function (b) {
    var vert = b[1];
    sbBind(b[0], {
      opts: function () { return saOpts(el, p.vp, vert); },
      set: function (v, phase) {
        if (phase === "end" || $el.hasClass(SB + "-disabled")) { return; }
        if (vert) { p.vp.scrollTop = v; } else { p.vp.scrollLeft = v; }
        saSync(el, $el);
      }
    });
  });
  $(p.vp).on("scroll" + NS, function () { saSync(el, $el); });
  var content = $(p.vp).children("." + SB + "-content")[0];
  sbObserve(el, [p.vp, content], function () { saLayout(el, $el); });
  saLayout(el, $el);
}

AH.define("scrollbar", {
  init: function (el, $el) {
    $.data(el, "ahScrollbar", {});
    if (el.hasAttribute("data-area")) { saArea(el, $el); } else { sbStandalone(el, $el); }
  },
  destroy: function (el) {
    var st = $.data(el, "ahScrollbar") || {};
    if (st.ro) { st.ro.disconnect(); }
    if (st.ns) { $(window).off(st.ns); }
    $.removeData(el, "ahScrollbar");
  },
  methods: {
    setValue: function (el, $el, v) {
      if (!el.hasAttribute("data-area")) { sbSet(el, $el, v, null); }
    },
    getValue: function (el) { return num(el, "data-ah-value", 0); },
    setMax: function (el, $el, max) {
      el.setAttribute("data-max", String(max));
      el.setAttribute("aria-valuemax", String(max));
      sbSet(el, $el, num(el, "data-ah-value", 0), null);
      sbLayout(sbBar($el), sbOpts(el));
    },
    scrollTo: function (el, $el, x, y) {
      var vp = saParts($el).vp;
      if (vp) {
        vp.scrollLeft = x || 0;
        vp.scrollTop = y || 0;
      }
    },
    refresh: function (el, $el) {
      if (el.hasAttribute("data-area")) { saLayout(el, $el); } else { sbLayout(sbBar($el), sbOpts(el)); }
    }
  }
});
