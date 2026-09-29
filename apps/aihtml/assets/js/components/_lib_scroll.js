/* Helpers the scrolling components share (scrollview.js, scrollbar.js,
 * responsive_panel.js): the value on the root and its hidden input,
 * numeric data attributes and pointer capture.
 *
 *   setValue(el, v)       data-ah-value and the hidden input
 *   num(el, attr, dflt)   a numeric attribute, dflt when missing
 *   capture(node, e)      pointer capture for a native pointer event
 */
import AH from "../core.js";

AH.lib = AH.lib || {};
AH.lib.scroll = {
  setValue: function (el, v) {
    el.setAttribute("data-ah-value", v);
    const hidden = el.querySelector(":scope > input[type=hidden]");
    if (hidden) { hidden.value = v; }
  },

  num: function (el, name, dflt) {
    const v = parseFloat(el.getAttribute(name));
    return isNaN(v) ? dflt : v;
  },

  capture: function (node, e) {
    const id = e && e.pointerId;
    if (id !== undefined && node.setPointerCapture) {
      try { node.setPointerCapture(id); } catch (err) { /* synthetic event */ }
    }
  }
};
