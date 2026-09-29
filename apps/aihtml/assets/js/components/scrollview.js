/* Behaviour of the scrollview component (designs/04-components.md),
 * ported from sigil's scrollview (cljs + jQuery). The value is kept in
 * data-ah-value on the root, mirrored into a hidden input when there is
 * one, and "change" fires on the root when the user changes it. Methods
 * called by the server (AH.invoke / aihtml_action:call) update the value
 * without firing "change". Drags use pointer capture, so nothing is bound
 * on document.
 */
import $ from "jquery";
import AH from "../core.js";
import "./_lib_scroll.js";

var NS = AH.NS;
var setValue = AH.lib.scroll.setValue, num = AH.lib.scroll.num, capture = AH.lib.scroll.capture;

// ------------------------------------------------------------------
// ScrollView: a horizontal pager (sigil layout/scrollview)
// ------------------------------------------------------------------
//
// Pages are 100% wide and the wrapper moves by margin-left in percent,
// so the layout follows the container without measuring; drags move it
// in pixels and the page change animates back to a percentage.

var SV = "ah-scrollview";
var SV_DEAD_ZONE = 15;

function svState(el) {
  var st = $.data(el, "ahScrollview");
  if (!st) {
    st = { timer: null, anim: null, paused: false };
    $.data(el, "ahScrollview", st);
  }
  return st;
}

function svWrapper($el) { return $el.children("." + SV + "-wrapper"); }
function svPages($el) { return svWrapper($el).children("." + SV + "-page"); }
function svIndex(el) { return parseInt(el.getAttribute("data-ah-value"), 10) || 0; }
function svDisabled($el) { return $el.hasClass(SV + "-disabled"); }

function svDuration(el) { return num(el, "data-duration", 300); }

// Turn the transition on for one move and off again when it ends.
function svAnimate(el, $el) {
  var st = svState(el);
  $el.addClass(SV + "-animating");
  clearTimeout(st.anim);
  st.anim = setTimeout(function () {
    st.anim = null;
    $el.removeClass(SV + "-animating");
  }, svDuration(el) + 50);
}

function svPlace($el, idx) {
  svWrapper($el)[0].style.marginLeft = idx ? (-idx * 100) + "%" : "";
}

function svMark($el, idx) {
  svPages($el).each(function (i) {
    var on = i === idx;
    if (on) {
      this.removeAttribute("aria-hidden");
      this.removeAttribute("inert");
    } else {
      this.setAttribute("aria-hidden", "true");
      this.setAttribute("inert", "");
    }
  });
  $el.children("." + SV + "-buttons").children("." + SV + "-button").each(function (i) {
    var on = i === idx;
    $(this).toggleClass(SV + "-button-active", on);
    if (on) { this.setAttribute("aria-current", "true"); } else { this.removeAttribute("aria-current"); }
  });
}

// how: "user" fires change, "api" and "auto" only ah:page-changed.
function svGo(el, $el, idx, how) {
  var cnt = svPages($el).length;
  if (!cnt) { return; }
  idx = Math.max(0, Math.min(cnt - 1, idx));
  var old = svIndex(el);
  svAnimate(el, $el);
  svPlace($el, idx);
  if (idx === old) { return; }
  svMark($el, idx);
  setValue(el, $el, String(idx));
  $el.trigger("ah:page-changed", [{ page: idx, old: old }]);
  if (how === "user") { $el.trigger("change"); }
}

function svStart(el, $el) {
  var st = svState(el);
  if (st.timer) { return; }
  st.timer = setInterval(function () {
    if (st.paused || svDisabled($el)) { return; }
    var cnt = svPages($el).length, cur = svIndex(el);
    svGo(el, $el, cur + 1 >= cnt ? 0 : cur + 1, "auto");
  }, num(el, "data-slide-duration", 3000));
}

function svStop(el) {
  var st = svState(el);
  clearInterval(st.timer);
  st.timer = null;
}

AH.define("scrollview", {
  init: function (el, $el) {
    var st = svState(el);
    var $w = svWrapper($el);
    var drag = null;
    svMark($el, svIndex(el));

    $w.on("pointerdown" + NS, function (e) {
      if (svDisabled($el) || (e.button !== undefined && e.button !== 0)) { return; }
      $el.removeClass(SV + "-animating");
      drag = { x: e.clientX, ml: parseFloat(getComputedStyle(this).marginLeft) || 0,
               moving: false, node: this, e: e };
    });
    $w.on("pointermove" + NS, function (e) {
      if (!drag) { return; }
      var dx = e.clientX - drag.x;
      if (!drag.moving) {
        if (Math.abs(dx) <= SV_DEAD_ZONE) { return; }
        drag.moving = true;
        capture(drag.node, drag.e);
        $el.addClass(SV + "-dragging");
      }
      e.preventDefault();
      var cw = $el.width(), cnt = svPages($el).length;
      var ml = drag.ml + dx;
      if (el.getAttribute("data-bounce") === "false") {
        ml = Math.max(-(cnt - 1) * cw, Math.min(0, ml));
      }
      this.style.marginLeft = ml + "px";
    });
    $w.on("pointerup" + NS + " pointercancel" + NS, function () {
      if (!drag) { return; }
      var d = drag;
      drag = null;
      if (!d.moving) { return; }
      $el.removeClass(SV + "-dragging");
      // swallow the click that ends a drag, so links in a page stay put
      $w.one("click" + NS, function (c) { c.preventDefault(); c.stopPropagation(); });
      setTimeout(function () { $w.off("click" + NS); }, 0);
      var dx = (parseFloat(this.style.marginLeft) || 0) - d.ml;
      var cw = $el.width(), threshold = num(el, "data-threshold", 0.5) * cw;
      var cur = svIndex(el), target = cur;
      if (dx < -threshold) { target = cur + 1; } else if (dx > threshold) { target = cur - 1; }
      svGo(el, $el, target, "user");
    });
    $w.on("dragstart" + NS, function (e) { e.preventDefault(); });

    $el.on("click" + NS, "." + SV + "-button", function () {
      if ($(this).closest("." + SV)[0] !== el || svDisabled($el)) { return; }
      svGo(el, $el, $(this).index(), "user");
    });

    $el.on("keydown" + NS, function (e) {
      if (e.target !== el || svDisabled($el)) { return; }
      var cur = svIndex(el), cnt = svPages($el).length, to = null;
      switch (e.key) {
        case "ArrowLeft": case "ArrowUp": case "PageUp": to = cur - 1; break;
        case "ArrowRight": case "ArrowDown": case "PageDown": to = cur + 1; break;
        case "Home": to = 0; break;
        case "End": to = cnt - 1; break;
        default: return;
      }
      e.preventDefault();
      svGo(el, $el, to, "user");
    });

    // the slide show waits while the pointer or the focus is inside
    $el.on("mouseenter" + NS + " focusin" + NS, function () { st.paused = true; });
    $el.on("mouseleave" + NS + " focusout" + NS, function (e) {
      if (e.type === "focusout" && e.relatedTarget && $.contains(el, e.relatedTarget)) { return; }
      st.paused = false;
    });
    if (el.hasAttribute("data-slide-show")) { svStart(el, $el); }
  },
  destroy: function (el) {
    var st = svState(el);
    svStop(el);
    clearTimeout(st.anim);
    $.removeData(el, "ahScrollview");
  },
  methods: {
    setValue: function (el, $el, v) { svGo(el, $el, parseInt(v, 10) || 0, "api"); },
    getValue: function (el) { return svIndex(el); },
    forward: function (el, $el) {
      var cur = svIndex(el);
      if (cur + 1 < svPages($el).length) { svGo(el, $el, cur + 1, "api"); }
    },
    back: function (el, $el) {
      var cur = svIndex(el);
      if (cur > 0) { svGo(el, $el, cur - 1, "api"); }
    },
    startSlideShow: function (el, $el) { svStart(el, $el); },
    stopSlideShow: function (el) { svStop(el); },
    refresh: function (el, $el) {
      var cnt = svPages($el).length, cur = Math.min(svIndex(el), Math.max(0, cnt - 1));
      setValue(el, $el, String(cur));
      svPlace($el, cur);
      svMark($el, cur);
    }
  }
});
