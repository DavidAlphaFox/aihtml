/* Internal: what the text entry behaviours share (input, password-input,
 * number-input, input-otp, tag-input). Ported from sigil. */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;

// Floating label: up while focused or filled (sigil util/sync-float-label!).
function syncLabel($el, prefix, filled, focused) {
  $el.find("." + prefix + "-label").toggleClass(prefix + "-label-float", !!(filled || focused));
}

// Focus class on the shell plus the floating label (sigil shell).
function focusShell($el, $input, prefix, after) {
  $input.on("focus" + NS, function () {
    $el.addClass(prefix + "-focused");
    syncLabel($el, prefix, true, true);
  }).on("blur" + NS, function () {
    $el.removeClass(prefix + "-focused");
    syncLabel($el, prefix, $input.val() !== "", false);
    if (after) { after(); }
  });
}

// Update data-ah-value and the hidden input; fire change on the root.
function commitValue($el, value) {
  if ($el.attr("data-ah-value") === value) { return false; }
  $el.attr("data-ah-value", value);
  $el.children("input[type=hidden]").val(value);
  $el.trigger("change");
  return true;
}

// Native events of the inner fields must not reach on(...) on the root:
// the root reports its own change.
function isolate($el, sel) {
  $el.on("change" + NS + " input" + NS, sel, function (e) { e.stopPropagation(); });
}

AH.lib = AH.lib || {};
AH.lib.input = {
  syncLabel: syncLabel,
  focusShell: focusShell,
  commitValue: commitValue,
  isolate: isolate
};
