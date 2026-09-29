/* Behaviour of the scrollbar component (designs/04-components.md),
 * ported from sigil's scrollbar (cljs + jQuery). A standalone bar keeps
 * its value in data-ah-value on the root, mirrors it into a hidden input
 * when there is one, and fires "change" on the root when the user changes
 * it; methods called by the server update the value without firing
 * "change". Drags use pointer capture, so nothing is bound on document.
 */
import AH from "../core.js";
import "./_lib_scroll.js";

const setValue = AH.lib.scroll.setValue, num = AH.lib.scroll.num, capture = AH.lib.scroll.capture;

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
  const part = function (name) { return bar.querySelector(":scope > ." + SB + "-" + name); };
  return {
    up: part("btn-up"),
    tu: part("track-up"),
    thumb: part("thumb"),
    td: part("track-down"),
    down: part("btn-down")
  };
}

function sbVertical(bar) { return bar.classList.contains(SB + "-vertical"); }

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
  const off = new AbortController();
  const end = function () {
    clearTimeout(t);
    clearInterval(iv);
    off.abort();
  };
  ["pointerup", "pointercancel", "lostpointercapture"].forEach(function (type) {
    node.addEventListener(type, end, { signal: off.signal });
  });
}

// Wire one bar. api: {opts(), set(value, phase), start()} where phase is
// "input" while dragging, "change" otherwise and "end" when a drag ends.
// ctrl is the controller (its listen removes the listeners on teardown).
function sbBind(ctrl, bar, api) {
  const p = sbParts(bar);
  [p.up, p.down].forEach(function (btn) {
    ctrl.listen(btn, "pointerdown", function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      const dir = btn === p.up ? -1 : 1;
      sbRepeat(btn, e, function () { const o = api.opts(); api.set(o.value + dir * o.step, "change"); });
    });
  });
  [p.tu, p.td].forEach(function (track) {
    ctrl.listen(track, "pointerdown", function (e) {
      if (e.button !== undefined && e.button !== 0) { return; }
      e.preventDefault();
      const o = api.opts();
      api.set(o.value + (track === p.tu ? -1 : 1) * o.large, "change");
    });
  });
  let drag = null;
  ctrl.listen(p.thumb, "pointerdown", function (e) {
    if (e.button !== undefined && e.button !== 0) { return; }
    e.preventDefault();
    const o = api.opts(), g = sbGeom(bar, o);
    drag = { start: g.vert ? e.clientY : e.clientX, pos: g.tp, vert: g.vert, value: o.value };
    capture(p.thumb, e);
    p.thumb.classList.add(SB + "-thumb-pressed");
    if (api.start) { api.start(); }
  });
  ctrl.listen(p.thumb, "pointermove", function (e) {
    if (!drag) { return; }
    const cur = drag.vert ? e.clientY : e.clientX;
    api.set(sbPosToValue(bar, api.opts(), drag.pos + cur - drag.start), "input");
  });
  const end = function () {
    if (!drag) { return; }
    const before = drag.value;
    drag = null;
    p.thumb.classList.remove(SB + "-thumb-pressed");
    api.set(api.opts().value, "end", before);
  };
  ctrl.listen(p.thumb, "pointerup", end);
  ctrl.listen(p.thumb, "pointercancel", end);
}

function sbObserve(ctrl, targets, fn) {
  if (window.ResizeObserver) {
    ctrl.ro = new ResizeObserver(function () { fn(); });
    targets.forEach(function (t) { if (t) { ctrl.ro.observe(t); } });
  } else {
    ctrl.listen(window, "resize", fn);
  }
}

function fire(el, type) {
  el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true }));
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

function sbBar(el) { return el.querySelector(":scope > ." + SB); }

function sbSet(el, v, event) {
  const o = sbOpts(el);
  v = Math.max(o.min, Math.min(o.max, +v || 0));
  if (sbIntegral(o)) { v = Math.round(v); }
  const changed = v !== o.value;
  if (changed) {
    setValue(el, String(v));
    el.setAttribute("aria-valuenow", String(v));
  }
  o.value = v;
  sbLayout(sbBar(el), o);
  if (changed && event) { fire(el, event); }
  return changed;
}

function sbStandalone(ctrl) {
  const el = ctrl.element;
  const bar = sbBar(el);
  const disabled = function () { return el.classList.contains(SB + "-disabled"); };
  sbBind(ctrl, bar, {
    opts: function () { return sbOpts(el); },
    set: function (v, phase, before) {
      if (disabled()) { return; }
      if (phase === "end") {
        if (String(before) !== el.getAttribute("data-ah-value")) { fire(el, "change"); }
        return;
      }
      sbSet(el, v, phase);
    },
    start: function () { el.focus({ preventScroll: true }); }
  });
  ctrl.listen(el, "keydown", function (e) {
    if (e.target !== el || disabled()) { return; }
    const o = sbOpts(el), vert = sbVertical(bar);
    let to;
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
    sbSet(el, to, "change");
  });
  const relayout = function () { sbLayout(bar, sbOpts(el)); };
  sbObserve(ctrl, [el], relayout);
  relayout();
}

// --- scroll area --------------------------------------------------------

function saParts(el) {
  return {
    vp: el.querySelector(":scope > ." + SB + "-viewport"),
    v: el.querySelector(":scope > ." + SB + "-vertical"),
    h: el.querySelector(":scope > ." + SB + "-horizontal")
  };
}

function saOpts(el, vp, vert) {
  const max = vert ? vp.scrollHeight - vp.clientHeight : vp.scrollWidth - vp.clientWidth;
  const page = vert ? vp.clientHeight : vp.clientWidth;
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
function saLayout(el) {
  const p = saParts(el), vp = p.vp;
  let needV = false, needH = false;
  for (let i = 0; i < 2; i++) {
    el.classList.toggle(SB + "-area-v", needV);
    el.classList.toggle(SB + "-area-h", needH);
    needV = vp.scrollHeight > vp.clientHeight + 1;
    needH = vp.scrollWidth > vp.clientWidth + 1;
  }
  el.classList.toggle(SB + "-area-v", needV);
  el.classList.toggle(SB + "-area-h", needH);
  if (needV) { sbLayout(p.v, saOpts(el, vp, true)); }
  if (needH) { sbLayout(p.h, saOpts(el, vp, false)); }
}

function saSync(el) {
  const p = saParts(el);
  if (el.classList.contains(SB + "-area-v")) { sbLayout(p.v, saOpts(el, p.vp, true)); }
  if (el.classList.contains(SB + "-area-h")) { sbLayout(p.h, saOpts(el, p.vp, false)); }
}

function saArea(ctrl) {
  const el = ctrl.element;
  const p = saParts(el);
  [[p.v, true], [p.h, false]].forEach(function (b) {
    const vert = b[1];
    sbBind(ctrl, b[0], {
      opts: function () { return saOpts(el, p.vp, vert); },
      set: function (v, phase) {
        if (phase === "end" || el.classList.contains(SB + "-disabled")) { return; }
        if (vert) { p.vp.scrollTop = v; } else { p.vp.scrollLeft = v; }
        saSync(el);
      }
    });
  });
  ctrl.listen(p.vp, "scroll", function () { saSync(el); });
  const content = p.vp.querySelector(":scope > ." + SB + "-content");
  sbObserve(ctrl, [p.vp, content], function () { saLayout(el); });
  saLayout(el);
}

AH.register("scrollbar", class extends AH.Controller {
  setup() {
    this.ro = null;
    if (this.element.hasAttribute("data-area")) { saArea(this); } else { sbStandalone(this); }
  }

  teardown() {
    if (this.ro) { this.ro.disconnect(); this.ro = null; }
  }

  // methods (aihtml_action:call/4, AH.invoke); they fire no change
  setValue(v) {
    if (!this.element.hasAttribute("data-area")) { sbSet(this.element, v, null); }
  }
  getValue() { return num(this.element, "data-ah-value", 0); }
  setMax(max) {
    const el = this.element;
    el.setAttribute("data-max", String(max));
    el.setAttribute("aria-valuemax", String(max));
    sbSet(el, num(el, "data-ah-value", 0), null);
    sbLayout(sbBar(el), sbOpts(el));
  }
  scrollTo(x, y) {
    const vp = saParts(this.element).vp;
    if (vp) {
      vp.scrollLeft = x || 0;
      vp.scrollTop = y || 0;
    }
  }
  refresh() {
    const el = this.element;
    if (el.hasAttribute("data-area")) { saLayout(el); } else { sbLayout(sbBar(el), sbOpts(el)); }
  }
});
