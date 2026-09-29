/* Helpers the scrolling components share (scrollview.js, scrollbar.js,
 * responsive_panel.js): the value on the root and its hidden input,
 * numeric data attributes and pointer capture.
 */
import $ from "jquery";
import AH from "../core.js";

AH.lib = AH.lib || {};
AH.lib.scroll = {
  // data-ah-value on the root and the hidden input mirroring it
  setValue: function (el, $el, v) {
    el.setAttribute("data-ah-value", v);
    $el.children("input[type=hidden]").val(v);
  },

  num: function (el, name, dflt) {
    var v = parseFloat(el.getAttribute(name));
    return isNaN(v) ? dflt : v;
  },

  capture: function (node, e) {
    var id = e.originalEvent && e.originalEvent.pointerId;
    if (id !== undefined && node.setPointerCapture) {
      try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
    }
  }
};
