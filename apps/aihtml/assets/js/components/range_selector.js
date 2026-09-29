/* Behaviour of range_selector (designs/04-components.md), after sigil's
 * form/range_selector.
 *
 *   range-selector   drag a marker or the bar between them; the markers
 *                    are ARIA sliders; input while dragging, change after
 *
 * The root keeps data-ah-value and its hidden input in step and fires
 * "input" / "change".
 *
 * The server renders the whole first state, so init only binds events.
 */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var seq = 0;

function docNS(el) {
  var ns = $.data(el, "ah-docns");
  if (!ns) {
    ns = NS + "er" + (++seq);
    $.data(el, "ah-docns", ns);
  }
  return ns;
}

function setValue($el, v) {
  $el.attr("data-ah-value", v);
  $el.children("input[type=hidden]").val(v);
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
