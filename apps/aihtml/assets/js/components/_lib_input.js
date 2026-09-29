/* Internal: what the text entry behaviours share (input, password-input,
 * number-input, input-otp, tag-input). Ported from sigil. The helpers
 * take elements; those that listen take the root's AH.Controller, so the
 * listeners go with it. */
import AH from "../core.js";

// Floating label: up while focused or filled (sigil util/sync-float-label!).
function syncLabel(el, prefix, filled, focused) {
  el.querySelectorAll("." + prefix + "-label").forEach(function (l) {
    l.classList.toggle(prefix + "-label-float", !!(filled || focused));
  });
}

// Focus class on the shell plus the floating label (sigil shell).
function focusShell(ctrl, input, prefix, after) {
  var el = ctrl.element;
  if (!input) { return; }
  ctrl.listen(input, "focus", function () {
    el.classList.add(prefix + "-focused");
    syncLabel(el, prefix, true, true);
  });
  ctrl.listen(input, "blur", function () {
    el.classList.remove(prefix + "-focused");
    syncLabel(el, prefix, input.value !== "", false);
    if (after) { after(); }
  });
}

// Update data-ah-value and the hidden input; fire change on the root
// (a native event, no detail).
function commitValue(el, value) {
  if (el.getAttribute("data-ah-value") === value) { return false; }
  el.setAttribute("data-ah-value", value);
  el.querySelectorAll(":scope > input[type=hidden]").forEach(function (h) { h.value = value; });
  el.dispatchEvent(new CustomEvent("change", { bubbles: true, cancelable: true }));
  return true;
}

// Native events of the inner fields must not reach on(...) on the root:
// the root reports its own change. Registered at setup, before the
// page's own listeners on the root, which see only the root's events.
function isolate(ctrl, sel) {
  var stop = function (e) { e.stopImmediatePropagation(); };
  ctrl.delegate("change", sel, stop);
  ctrl.delegate("input", sel, stop);
}

// A native event on an inner field (what the browser would send), so
// on(input|change, ...) and the page's listeners hear it.
function emit(target, type) {
  target.dispatchEvent(new Event(type, { bubbles: true, cancelable: type !== "input" && type !== "change" }));
}

AH.lib = AH.lib || {};
AH.lib.input = {
  syncLabel: syncLabel,
  focusShell: focusShell,
  commitValue: commitValue,
  isolate: isolate,
  emit: emit
};
