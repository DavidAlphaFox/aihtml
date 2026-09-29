/* Shared by the popup pickers (colorpicker.js, timepicker.js, ported from
 * sigil): the popup a field toggles, pointer drags and small helpers.
 * `cls` is the component's root class ("ah-timepicker"); its popup is
 * the root's child `.<cls>-popup` and the root gets `<cls>-open` while it
 * shows. */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var seq = 0;

function uid(prefix) {
  seq += 1;
  return prefix + seq + "-" + Math.random().toString(36).slice(2, 7);
}

// A number per picker instance (data-ah-uid: the namespace of its
// document handlers), shared by all pickers so they never clash.
function nextId() { return ++seq; }

function clamp(v, lo, hi) {
  return Math.max(lo, Math.min(hi, v));
}

function round2(n) { return Math.round(n * 100) / 100; }

function disabled(el) {
  return el.getAttribute("aria-disabled") === "true";
}

// Keep the inner fields' native events inside the component.
function fenceNativeEvents($el) {
  $el.on("input" + NS + " change" + NS, "input", function (e) {
    e.stopPropagation();
  });
}

// ------------------------------------------------------------------
// Popup: a field toggles a panel placed by AH.float (below, above when
// there is no room), closed by Escape or a press outside.
// ------------------------------------------------------------------

function popupOf($el, cls) {
  return $el.children("." + cls + "-popup").first();
}

function isOpen($el, cls) {
  var $p = popupOf($el, cls);
  return $p.length > 0 && !$p.prop("hidden");
}

function openPopup(el, $el, cls, $opener, onOpen) {
  var $p = popupOf($el, cls);
  if (!$p.length || !$p.prop("hidden") || disabled(el)) { return; }
  if (onOpen) { onOpen(); }
  $p.prop("hidden", false);
  $el.addClass(cls + "-open");
  $opener.attr("aria-expanded", "true");
  // Fixed positioning at the field (the input area or the trigger), so
  // an overflow:hidden ancestor such as a card does not clip it; flips
  // above when there is no room below and follows scrolling.
  $.data(el, "ah-float", AH.float($p[0], $el.children().first()[0],
                                  { placement: "bottom", align: "start", offset: 4 }));
  var ns = ".ahpop" + el.getAttribute("data-ah-uid");
  $(document).off(ns).on("mousedown" + ns + " touchstart" + ns + " focusin" + ns, function (e) {
    if (!$.contains(el, e.target) && e.target !== el) {
      closePopup(el, $el, cls, $opener, false);
    }
  });
}

function closePopup(el, $el, cls, $opener, refocus) {
  var $p = popupOf($el, cls);
  stopFloat(el);
  if (!$p.length || $p.prop("hidden")) { return; }
  $p.prop("hidden", true);
  $el.removeClass(cls + "-open");
  $opener.attr("aria-expanded", "false");
  if (refocus) { $opener.trigger("focus"); }
}

// Unbind the outside-press handler and stop the float (close, destroy).
function stopFloat(el) {
  $(document).off(".ahpop" + el.getAttribute("data-ah-uid"));
  var h = $.data(el, "ah-float");
  if (h) { h.stop(); $.removeData(el, "ah-float"); }
}

function pointerXY(e) {
  var oe = e.originalEvent || e;
  var t = oe.touches && oe.touches[0] ? oe.touches[0] : oe;
  return { x: t.clientX, y: t.clientY };
}

// Pointer drag on one element: start(e), move(e), end(e). Uses pointer
// capture, so nothing is bound on document.
function drag($target, el, start, move, end) {
  $target.on("pointerdown" + NS, function (e) {
    if (disabled(el) || (e.button !== undefined && e.button !== 0)) { return; }
    if (start(e) === false) { return; }
    e.preventDefault();
    var node = this;
    var id = e.originalEvent && e.originalEvent.pointerId;
    if (id !== undefined && node.setPointerCapture) {
      try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
    }
    node.focus && node.focus({ preventScroll: true });
    var $n = $(node);
    $n.on("pointermove" + NS + "drag", function (m) { move(m); });
    $n.on("pointerup" + NS + "drag pointercancel" + NS + "drag", function (u) {
      $n.off(NS + "drag");
      end(u);
    });
  });
}

AH.lib = AH.lib || {};
AH.lib.picker = {
  uid: uid,
  nextId: nextId,
  clamp: clamp,
  round2: round2,
  disabled: disabled,
  fenceNativeEvents: fenceNativeEvents,
  popupOf: popupOf,
  isOpen: isOpen,
  openPopup: openPopup,
  closePopup: closePopup,
  stopFloat: stopFloat,
  pointerXY: pointerXY,
  drag: drag
};
