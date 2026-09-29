/* Shared by the popup pickers (colorpicker.js, timepicker.js, ported from
 * sigil): the popup a field toggles, pointer drags and small helpers.
 * `cls` is the component's root class ("ah-timepicker"); its popup is
 * the root's child `.<cls>-popup` and the root gets `<cls>-open` while it
 * shows. Functions taking `ctl` take the component's AH.Controller: the
 * popup's float handle and outside-press listeners live on it. */
import AH from "../core.js";

var seq = 0;

function uid(prefix) {
  seq += 1;
  return prefix + seq + "-" + Math.random().toString(36).slice(2, 7);
}

function clamp(v, lo, hi) {
  return Math.max(lo, Math.min(hi, v));
}

function round2(n) { return Math.round(n * 100) / 100; }

function disabled(el) {
  return el.getAttribute("aria-disabled") === "true";
}

// Keep the inner fields' native input/change events inside the component
// (the root's own events have the root as target).
function fenceNativeEvents(ctl) {
  var el = ctl.element;
  var stop = function (e) {
    if (e.target !== el && e.target.matches && e.target.matches("input")) { e.stopPropagation(); }
  };
  ctl.listen(el, "input", stop);
  ctl.listen(el, "change", stop);
}

// ------------------------------------------------------------------
// Popup: a field toggles a panel placed by AH.float (below, above when
// there is no room), closed by Escape or a press outside.
// ------------------------------------------------------------------

function popupOf(el, cls) {
  for (var c = el.firstElementChild; c; c = c.nextElementSibling) {
    if (c.classList.contains(cls + "-popup")) { return c; }
  }
  return null;
}

function isOpen(el, cls) {
  var p = popupOf(el, cls);
  return !!p && !p.hidden;
}

function openPopup(ctl, cls, opener, onOpen) {
  var el = ctl.element;
  var p = popupOf(el, cls);
  if (!p || !p.hidden || disabled(el)) { return; }
  if (onOpen) { onOpen(); }
  p.hidden = false;
  el.classList.add(cls + "-open");
  if (opener) { opener.setAttribute("aria-expanded", "true"); }
  stopFloat(ctl);
  // Fixed positioning at the field (the input area or the trigger), so
  // an overflow:hidden ancestor such as a card does not clip it; flips
  // above when there is no room below and follows scrolling.
  ctl._pickFloat = AH.float(p, el.firstElementChild, { placement: "bottom", align: "start", offset: 4 });
  var ac = new AbortController();
  ctl._pickOutside = ac;
  var outside = function (e) {
    if (!el.contains(e.target)) { closePopup(ctl, cls, opener, false); }
  };
  ["mousedown", "touchstart", "focusin"].forEach(function (t) {
    document.addEventListener(t, outside, { signal: ac.signal });
  });
}

function closePopup(ctl, cls, opener, refocus) {
  var el = ctl.element;
  var p = popupOf(el, cls);
  stopFloat(ctl);
  if (!p || p.hidden) { return; }
  p.hidden = true;
  el.classList.remove(cls + "-open");
  if (opener) {
    opener.setAttribute("aria-expanded", "false");
    if (refocus) { opener.focus(); }
  }
}

// Remove the outside-press listeners and stop the float (close, teardown).
function stopFloat(ctl) {
  if (ctl._pickOutside) { ctl._pickOutside.abort(); ctl._pickOutside = null; }
  if (ctl._pickFloat) { ctl._pickFloat.stop(); ctl._pickFloat = null; }
}

function pointerXY(e) {
  var t = e.touches && e.touches[0] ? e.touches[0] : e;
  return { x: t.clientX, y: t.clientY };
}

// Pointer drag on one element: start(e), move(e), end(e). Uses pointer
// capture, so nothing is bound on document.
function drag(ctl, target, start, move, end) {
  if (!target) { return; }
  var el = ctl.element;
  ctl.listen(target, "pointerdown", function (e) {
    if (disabled(el) || (e.button !== undefined && e.button !== 0)) { return; }
    if (start(e) === false) { return; }
    e.preventDefault();
    if (e.pointerId !== undefined && target.setPointerCapture) {
      try { target.setPointerCapture(e.pointerId); } catch (err) { /* synthetic event */ }
    }
    if (target.focus) { target.focus({ preventScroll: true }); }
    var ac = new AbortController();
    var up = function (u) { ac.abort(); end(u); };
    target.addEventListener("pointermove", function (m) { move(m); }, { signal: ac.signal });
    target.addEventListener("pointerup", up, { signal: ac.signal });
    target.addEventListener("pointercancel", up, { signal: ac.signal });
  });
}

AH.lib = AH.lib || {};
AH.lib.picker = {
  uid: uid,
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
