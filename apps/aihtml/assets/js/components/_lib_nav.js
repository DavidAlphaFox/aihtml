/* Shared by the navigation behaviours (menu, navbar, sidenav, toolbar,
   splitter, listmenu, tabs): AH.lib.nav. Ported from sigil's
   components/layout/*.cljs.

   API (elements, never jQuery objects):
     visible(el)                         laid out (jQuery's :visible)
     setValue(el, v[, eventName])        data-ah-value, the hidden input, then
                                         a native bubbling event on el
     byKey(scope, sel, attr, key)        the elements under scope (an element
                                         or an array of them) matching sel
                                         whose attr equals key (an array)
     hover(ctrl, sel, enter, leave[, root])
                                         delegated mouseenter / mouseleave on
                                         root (default ctrl.element; ctrl is
                                         the AH.Controller), as
                                         jQuery's .on("mouseenter", sel, fn):
                                         enter(match, e) / leave(match, e) */
import AH from "../core.js";

function visible(el) {
  return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length);
}

function setValue(el, v, eventName) {
  v = v === null || v === undefined ? "" : String(v);
  el.setAttribute("data-ah-value", v);
  const hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
  if (eventName) {
    el.dispatchEvent(new CustomEvent(eventName, { bubbles: true, cancelable: true }));
  }
}

function byKey(scope, sel, attr, key) {
  const roots = Array.isArray(scope) ? scope : [scope];
  const out = [];
  roots.forEach(function (root) {
    root.querySelectorAll(sel).forEach(function (n) {
      if (n.getAttribute(attr) === String(key)) { out.push(n); }
    });
  });
  return out;
}

// mouseover / mouseout that cross the boundary of a match are its
// mouseenter / mouseleave; nested matches each get theirs, innermost
// first (as jQuery's delegation does).
function hover(ctrl, sel, enter, leave, scope) {
  const root = scope || ctrl.element;
  const edges = function (e) {
    const out = [];
    const rel = e.relatedTarget;
    for (let n = e.target; n && n !== root && n.nodeType === 1; n = n.parentNode) {
      if (n.matches(sel) && !(rel && (rel === n || n.contains(rel)))) { out.push(n); }
    }
    return root.contains(e.target) ? out : [];
  };
  if (enter) {
    ctrl.listen(root, "mouseover", function (e) {
      edges(e).forEach(function (hit) { enter(hit, e); });
    });
  }
  if (leave) {
    ctrl.listen(root, "mouseout", function (e) {
      edges(e).forEach(function (hit) { leave(hit, e); });
    });
  }
}

AH.lib = AH.lib || {};
AH.lib.nav = {
  visible: visible,
  setValue: setValue,
  byKey: byKey,
  hover: hover
};
