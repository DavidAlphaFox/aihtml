/* Behaviour of the splitter component (designs/04-components.md), ported
   from sigil's components/layout/*.cljs; shared helpers in _lib_nav.js. */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_nav.js";

var NS = AH.NS;
var L = AH.lib.nav;
var setValue = L.setValue;

// ------------------------------------------------------------------
// splitter (sigil splitter.cljs)
// ------------------------------------------------------------------
//
// The first pane's flex-basis is a fraction of the space left by the
// bar, so the split keeps its proportion when the container resizes.

function spState(el) {
  var st = $.data(el, "ah-splitter");
  if (st) { return st; }
  var $el = $(el);
  var horiz = $el.hasClass("ah-splitter-horizontal");
  var mins = (el.getAttribute("data-ah-min") || "0,0").split(",").map(function (x) {
    return parseFloat(x) || 0;
  });
  st = {
    horiz: horiz,
    $p: $el.children(".ah-splitter-panel"),
    $bar: $el.children(".ah-splitter-splitbar"),
    min0: mins[0],
    min1: mins[1] || 0,
    frac: null,
    saved: null
  };
  $.data(el, "ah-splitter", st);
  return st;
}

function spDim(st, node) { return st.horiz ? node.offsetHeight : node.offsetWidth; }
function spAvail(el, st) { return spDim(st, el) - spDim(st, st.$bar[0]); }

function spFormat(f) {
  var a = Math.round(f * 1000) / 10;
  var b = Math.round((100 - a) * 10) / 10;
  return a + "," + b;
}

function spApply(el, st, frac) {
  st.frac = Math.max(0, Math.min(1, frac));
  var bar = spDim(st, st.$bar[0]);
  st.$p.eq(0).css("flex", "0 0 calc((100% - " + bar + "px) * " + st.frac.toFixed(5) + ")");
  st.$bar.attr("aria-valuenow", Math.round(st.frac * 100));
}

// Resize pane 0 to px (clamped to the minimum sizes).
function spResize(el, st, px) {
  var avail = spAvail(el, st);
  if (avail <= 0) { return; }
  var clamped = Math.max(st.min0, Math.min(avail - st.min1, px));
  spApply(el, st, clamped / avail);
}

function spCollapsed(el, st, on) {
  $(el).toggleClass("ah-splitter-collapsed", on);
  st.$p.eq(0).css(st.horiz ? "min-height" : "min-width", on ? "0px" : st.min0 + "px");
}

function spToggle(el, st) {
  if ($(el).hasClass("ah-splitter-collapsed")) {
    spCollapsed(el, st, false);
    spApply(el, st, st.saved !== null ? st.saved : 0.5);
    $(el).trigger("ah:expanded");
  } else {
    st.saved = st.frac;
    spCollapsed(el, st, true);
    spApply(el, st, 0);
    $(el).trigger("ah:collapsed");
  }
  setValue($(el), spFormat(st.frac), "change");
}

function spEnabled(el) {
  return !$(el).hasClass("ah-splitter-disabled") && el.getAttribute("data-ah-resizable") !== "false";
}

AH.define("splitter", {
  init: function (el, $el) {
    var st = spState(el);
    var avail = spAvail(el, st);
    // measure the initial split (pixels or percent) as a fraction
    if (avail > 0) {
      var v = el.getAttribute("data-ah-value");
      spApply(el, st, v ? parseFloat(v) / 100 : spDim(st, st.$p[0]) / avail);
    }
    var drag = null;
    st.$bar.on("pointerdown" + NS, function (e) {
      if (e.button !== 0 || !spEnabled(el) || $(e.target).closest(".ah-splitter-collapse-btn").length) {
        return;
      }
      e.preventDefault();
      if (this.setPointerCapture) { this.setPointerCapture(e.pointerId); }
      drag = { start: st.horiz ? e.clientY : e.clientX, size: spDim(st, st.$p[0]),
               value: el.getAttribute("data-ah-value") };
      if ($el.hasClass("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
      $el.addClass("ah-splitter-dragging");
      $el.trigger("ah:resize-start");
    });
    st.$bar.on("pointermove" + NS, function (e) {
      if (!drag) { return; }
      var want = drag.size + (st.horiz ? e.clientY : e.clientX) - drag.start;
      var max = spAvail(el, st) - st.min1;
      st.$bar.toggleClass("ah-splitbar-invalid", want <= st.min0 || want >= max);
      spResize(el, st, want);
      setValue($el, spFormat(st.frac), "input");
    });
    st.$bar.on("pointerup" + NS + " pointercancel" + NS, function () {
      if (!drag) { return; }
      var before = drag.value;
      drag = null;
      st.$bar.removeClass("ah-splitbar-invalid");
      $el.removeClass("ah-splitter-dragging");
      var v = spFormat(st.frac);
      if (v !== before) { setValue($el, v, "change"); }
      $el.trigger("ah:resize");
    });
    st.$bar.on("click" + NS, ".ah-splitter-collapse-btn", function (e) {
      e.stopPropagation();
      if (spEnabled(el) || $el.hasClass("ah-splitter-collapsed")) { spToggle(el, st); }
    });
    st.$bar.on("keydown" + NS, function (e) {
      if (e.target !== this || !spEnabled(el)) { return; }
      var step = (parseInt(el.getAttribute("data-ah-step"), 10) || 10) * (e.shiftKey ? 5 : 1);
      var cur = spDim(st, st.$p[0]);
      var dec = st.horiz ? "ArrowUp" : "ArrowLeft";
      var inc = st.horiz ? "ArrowDown" : "ArrowRight";
      var px;
      switch (e.key) {
        case dec: px = cur - step; break;
        case inc: px = cur + step; break;
        case "Home": px = 0; break;
        case "End": px = Infinity; break;
        case "Enter": e.preventDefault(); spToggle(el, st); return;
        default: return;
      }
      e.preventDefault();
      if ($el.hasClass("ah-splitter-collapsed")) { spCollapsed(el, st, false); }
      spResize(el, st, px);
      var v = spFormat(st.frac);
      if (v !== el.getAttribute("data-ah-value")) { setValue($el, v, "change"); }
    });
  },
  destroy: function (el) {
    $.removeData(el, "ah-splitter");
  },
  methods: {
    // sizes: pane 0 in percent (a number or "30")
    setSizes: function (el, $el, pct) {
      var st = spState(el);
      spCollapsed(el, st, false);
      spApply(el, st, parseFloat(pct) / 100);
      setValue($el, spFormat(st.frac));
    },
    getSizes: function (el) {
      var st = spState(el);
      return [spDim(st, st.$p[0]), spDim(st, st.$p[1])];
    },
    collapse: function (el) {
      if (!$(el).hasClass("ah-splitter-collapsed")) { spToggle(el, spState(el)); }
    },
    expand: function (el) {
      if ($(el).hasClass("ah-splitter-collapsed")) { spToggle(el, spState(el)); }
    }
  }
});
