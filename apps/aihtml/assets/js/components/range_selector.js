/* Behaviour of range_selector (designs/04-components.md), after sigil's
 * form/range_selector.
 *
 *   range-selector   drag a marker or the bar between them; the markers
 *                    are ARIA sliders; input while dragging, change after
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change" (native events, detail: the value "lo,hi").
 *
 * The server renders the whole first state, so setup only binds events.
 */
import AH from "../core.js";

function writeValue(el, v) {
  el.setAttribute("data-ah-value", v);
  el.querySelectorAll(":scope > input[type=hidden]").forEach(function (h) { h.value = v; });
}

function child(el, cls) {
  return el ? Array.from(el.children).find(function (c) { return c.classList.contains(cls); }) || null : null;
}

// ------------------------------------------------------------------
// range-selector
// ------------------------------------------------------------------

var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

function erlRound(v) { return v < 0 ? -Math.round(-v) : Math.round(v); }

// The same formats as aihtml_range_selector:format/2.
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

function rsSnap(st, v) {
  v = st.min + Math.round((v - st.min) / st.step) * st.step;
  return tidy(Math.max(st.min, Math.min(st.max, v)));
}

function pct(st, v) { return tidy((v - st.min) / (st.max - st.min) * 100); }

function pointX(e) {
  var t = e.touches && e.touches[0] || e.changedTouches && e.changedTouches[0];
  return (t || e).clientX;
}

function css(node, props) {
  if (node) { Object.assign(node.style, props); }
}

AH.register("range-selector", class extends AH.Controller {
  setup() {
    var el = this.element;
    var num = function (a) { return parseFloat(el.getAttribute(a)); };
    var v = (el.getAttribute("data-ah-value") || "").split(",");
    var track = child(el, "ah-range-selector-track");
    var st = this.st = {
      min: num("data-ah-min"), max: num("data-ah-max"), step: num("data-ah-step") || 1,
      page: num("data-ah-page") || 10, minSpan: num("data-ah-min-span") || 0,
      format: JSON.parse(el.getAttribute("data-ah-format") || "{}"),
      lo: parseFloat(v[0]), hi: parseFloat(v[1]),
      committed: el.getAttribute("data-ah-value"),
      track: track,
      slider: child(track, "ah-range-selector-slider"),
      shutL: child(track, "ah-range-selector-shutter-left"),
      shutR: child(track, "ah-range-selector-shutter-right"),
      mL: child(track, "ah-range-selector-marker-left"),
      mR: child(track, "ah-range-selector-marker-right"),
      drag: null
    };
    if (!track) { return; }

    var onMarker = (e, marker) => {
      if (this.disabled()) { return; }
      var left = marker.classList.contains("ah-range-selector-marker-left");
      marker.focus();
      this.drag(e, (x) => { this.moveEnd(left, this.valueAt(x), false); });
    };
    this.delegate("mousedown", ".ah-range-selector-marker", onMarker, track);
    this.delegate("touchstart", ".ah-range-selector-marker", onMarker, track);
    if (st.slider) {
      var onBar = (e) => {
        if (this.disabled()) { return; }
        var span = tidy(st.hi - st.lo), grab = this.valueAt(pointX(e)) - st.lo;
        this.drag(e, (x) => {
          var lo = rsSnap(st, Math.max(st.min, Math.min(st.max - span, this.valueAt(x) - grab)));
          this.set(lo, Math.min(st.max, tidy(lo + span)), false);
        });
      };
      this.listen(st.slider, "mousedown", onBar);
      this.listen(st.slider, "touchstart", onBar);
    }
    this.delegate("keydown", ".ah-range-selector-marker", (e, marker) => {
      if (this.disabled()) { return; }
      var left = marker.classList.contains("ah-range-selector-marker-left"), cur = left ? st.lo : st.hi, to;
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
      this.moveEnd(left, to, true);
    }, track);
  }

  teardown() { this.endDrag(); }

  // methods (aihtml_action:call/4, AH.invoke)
  setValue(v) {
    var st = this.st;
    if (typeof v === "string") { v = v.split(","); }
    var lo = rsSnap(st, parseFloat(v[0])), hi = rsSnap(st, parseFloat(v[1]));
    if (lo > hi) { var t = lo; lo = hi; hi = t; }
    st.lo = lo; st.hi = hi;
    this.layout();
    st.committed = this.element.getAttribute("data-ah-value");
  }
  getValue() { return [this.st.lo, this.st.hi]; }

  disabled() { return this.element.classList.contains("ah-range-selector-disabled"); }

  layout() {
    var st = this.st, a = pct(st, st.lo), b = pct(st, st.hi);
    css(st.slider, { left: a + "%", width: tidy(b - a) + "%" });
    css(st.shutL, { width: a + "%" });
    css(st.shutR, { left: b + "%", width: tidy(100 - b) + "%" });
    [[st.mL, st.lo, a], [st.mR, st.hi, b]].forEach(function (m) {
      if (!m[0]) { return; }
      var t = rsFormat(m[1], st.format);
      m[0].style.left = m[2] + "%";
      m[0].setAttribute("aria-valuenow", String(m[1]));
      m[0].setAttribute("aria-valuetext", t);
      var label = child(m[0], "ah-range-selector-marker-value");
      if (label) { label.textContent = t; }
    });
    writeValue(this.element, st.lo + "," + st.hi);
  }

  // Set lo / hi (already bounded); input when it changed, change if asked.
  set(lo, hi, change) {
    var st = this.st, el = this.element, old = el.getAttribute("data-ah-value");
    st.lo = lo; st.hi = hi;
    this.layout();
    var v = el.getAttribute("data-ah-value");
    if (v !== old) { this.fire("input", v); }
    if (change && v !== st.committed) {
      st.committed = v;
      this.fire("change", v);
    }
  }

  // Move one end to v, kept min_span away from the other.
  moveEnd(left, v, change) {
    var st = this.st;
    v = rsSnap(st, v);
    if (left) {
      this.set(Math.max(st.min, Math.min(v, tidy(st.hi - st.minSpan))), st.hi, change);
    } else {
      this.set(st.lo, Math.min(st.max, Math.max(v, tidy(st.lo + st.minSpan))), change);
    }
  }

  valueAt(x) {
    var st = this.st, r = st.track.getBoundingClientRect();
    var p = r.width > 0 ? Math.max(0, Math.min(1, (x - r.left) / r.width)) : 0;
    return st.min + p * (st.max - st.min);
  }

  endDrag() {
    if (this.st && this.st.drag) { this.st.drag.abort(); this.st.drag = null; }
  }

  // Follow the pointer on the document until release.
  drag(e, move) {
    var st = this.st, el = this.element;
    if (e.type === "mousedown") {
      if (e.button !== 0) { return; }
      e.preventDefault();
    }
    this.endDrag();
    var ac = st.drag = new AbortController(), o = { signal: ac.signal };
    var onMove = function (me) {
      if (me.type === "mousemove") { me.preventDefault(); }
      move(pointX(me));
    };
    var onUp = () => {
      this.endDrag();
      el.classList.remove("ah-range-selector-dragging");
      this.set(st.lo, st.hi, true);
    };
    document.addEventListener("mousemove", onMove, o);
    document.addEventListener("touchmove", onMove, o);
    ["mouseup", "touchend", "touchcancel"].forEach(function (t) { document.addEventListener(t, onUp, o); });
    el.classList.add("ah-range-selector-dragging");
  }
});
