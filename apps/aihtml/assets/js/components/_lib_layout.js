/* Helpers shared by the layout components (panel, expander, tabs,
 * tab-bar, pagination, steps, loader, navigationbar, responsive-panel):
 * AH.lib.layout.
 *
 * Value-bearing components keep their value in data-ah-value on the root,
 * mirror it into a hidden input when there is one, and fire "change" on
 * the root when the user changes it. Methods called by the server
 * (AH.invoke / aihtml_action:call) update the value without firing
 * "change".
 *
 * API (elements, never jQuery objects):
 *   setValue(el, v)                     data-ah-value and the hidden input
 *   key(e)                              the key of a keyboard event
 *   nextEnabled(items, start, dir, cls) next index without class cls, wrapping
 *   listKeys(e, items, cur, vertical, cls)
 *                                       the index an arrow/Home/End/Enter/
 *                                       Space key moves to, or null
 *   show(el) / hide(el)                 inline display, as jQuery's show/hide
 *   slide(el, open, ms, done)           jQuery's slideDown / slideUp
 *   fade(el, open, ms, done)            jQuery's fadeIn / fadeOut
 *   animate(el, keyframes, ms, done)    a Web Animation that stop(el) can cut
 *                                       short (jQuery's .animate)
 *   stop(el)                            jQuery's .stop(true, true): the running
 *                                       slide/fade/animate jumps to its end and
 *                                       its callback runs now
 */
import AH from "../core.js";

function setValue(el, v) {
  el.setAttribute("data-ah-value", v);
  const hidden = el.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
}

function key(e) {
  return e.key;
}

// Next enabled index from start in direction dir, wrapping.
function nextEnabled(items, start, dir, disabledCls) {
  const n = items.length;
  for (let s = 1, i = (start + dir + n) % n; s <= n; s++, i = (i + dir + n) % n) {
    if (!items[i].classList.contains(disabledCls)) { return i; }
  }
  return start;
}

// Arrow keys (by orientation), Home, End; Enter/Space activate.
function listKeys(e, items, cur, vertical, disabledCls) {
  const k = key(e);
  const prev = vertical ? "ArrowUp" : "ArrowLeft";
  const next = vertical ? "ArrowDown" : "ArrowRight";
  if (k === "Home") { return nextEnabled(items, -1, 1, disabledCls); }
  if (k === "End") { return nextEnabled(items, items.length, -1, disabledCls); }
  if (k === prev) { return nextEnabled(items, cur, -1, disabledCls); }
  if (k === next) { return nextEnabled(items, cur, 1, disabledCls); }
  if (k === "Enter" || k === " ") { return cur; }
  return null;
}

// ---- show / hide / slide / fade -------------------------------------

function hidden(el) {
  return getComputedStyle(el).display === "none";
}

function show(el) {
  el.style.display = "";
  if (hidden(el)) { el.style.display = "block"; }
}

function hide(el) {
  el.style.display = "none";
}

const running = new WeakMap();

function stop(el) {
  const r = running.get(el);
  if (r) { r(); }
}

// Run keyframes on el for ms; end() runs once, when the animation ends
// or when stop(el) cuts it short (synchronously then).
function run(el, frames, ms, end) {
  stop(el);
  let over = false;
  const anim = ms > 0 && el.animate
    ? el.animate(frames, { duration: ms, easing: "ease-in-out", fill: "forwards" }) : null;
  const finish = function () {
    if (over) { return; }
    over = true;
    running.delete(el);
    end();
    if (anim) { anim.cancel(); }
  };
  running.set(el, finish);
  if (anim) { anim.onfinish = finish; } else { finish(); }
}

const SLIDE_PROPS = ["height", "paddingTop", "paddingBottom", "marginTop", "marginBottom"];

function slide(el, open, ms, done) {
  stop(el);
  const cb = done || function () {};
  if (open === !hidden(el)) { cb(); return; }
  if (open) { show(el); }
  const cs = getComputedStyle(el);
  const full = {};
  const zero = {};
  SLIDE_PROPS.forEach(function (p) {
    full[p] = p === "height" ? el.getBoundingClientRect().height + "px" : cs[p];
    zero[p] = "0px";
  });
  const overflow = el.style.overflow;
  el.style.overflow = "hidden";
  run(el, open ? [zero, full] : [full, zero], ms, function () {
    el.style.overflow = overflow;
    if (!open) { hide(el); }
    cb();
  });
}

function fade(el, open, ms, done) {
  stop(el);
  const cb = done || function () {};
  if (open === !hidden(el)) { cb(); return; }
  if (open) { show(el); }
  const from = getComputedStyle(el).opacity;
  run(el, open ? [{ opacity: 0 }, { opacity: from }] : [{ opacity: from }, { opacity: 0 }], ms,
      function () {
        if (!open) { hide(el); }
        cb();
      });
}

AH.lib = AH.lib || {};
AH.lib.layout = {
  setValue: setValue, key: key, nextEnabled: nextEnabled, listKeys: listKeys,
  show: show, hide: hide, slide: slide, fade: fade, animate: run, stop: stop
};
