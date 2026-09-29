/* Shared by the navigation behaviours (menu, navbar, sidenav, toolbar,
   splitter, listmenu): AH.lib.nav. Ported from sigil's
   components/layout/*.cljs. */
import $ from "jquery";
import AH from "../core.js";

var NS = AH.NS;
var uid = 0;

// A per-instance namespace for document/window handlers, which
// AH.destroy does not remove by itself.
function instanceNs(el) {
  var ns = $.data(el, "ah-ns");
  if (!ns) {
    ns = NS + "-nav" + (++uid);
    $.data(el, "ah-ns", ns);
  }
  return ns;
}

function visible(el) {
  return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
}

// Value contract: data-ah-value on the root, the hidden input, then a
// jQuery event (change or input) on the root.
function setValue($el, v, eventName) {
  v = v === null || v === undefined ? "" : String(v);
  $el.attr("data-ah-value", v);
  $el.children("input[type=hidden]").val(v);
  if (eventName) { $el.trigger(eventName); }
}

function byKey($scope, sel, attr, key) {
  return $scope.find(sel).filter(function () {
    return this.getAttribute(attr) === String(key);
  });
}

AH.lib = AH.lib || {};
AH.lib.nav = {
  instanceNs: instanceNs,
  visible: visible,
  setValue: setValue,
  byKey: byKey
};
