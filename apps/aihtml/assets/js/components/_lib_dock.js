/* Internal: what the docking behaviours share (docking.js,
 * dock_layout.js), ported from sigil's layout/docking and
 * layout/dock_layout. Pointer events (mouse, pen, touch) instead of
 * sigil's mouse sequences; one drag at a time per component, tracked on
 * the document and unbound when it ends or the component is torn down.
 * The arrangement is the value: JSON in data-ah-value (and the hidden
 * input), rewritten after every change (commit).
 */
import AH from "../core.js";

const DISTANCE = 5;          // px of movement before a press becomes a drag
const drags = new WeakMap(); // component root -> stop() of its drag

AH.lib = AH.lib || {};
// seq: one counter for the ids of new groups, float windows and menus
// (next())
const L = AH.lib.dock = { seq: 0 };

function inside(x, y, r) {
  return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
}

function parse(json) {
  try { return JSON.parse(json || "null"); } catch (e) { return null; }
}

function emit(el, type, detail) {
  return el.dispatchEvent(new CustomEvent(type, { bubbles: true, cancelable: true, detail: detail }));
}

// The element children of node matching selector.
function kids(node, selector) {
  return node ? Array.from(node.children).filter((c) => !selector || c.matches(selector)) : [];
}

function commit(el, json, fire) {
  if (json === el.getAttribute("data-ah-value")) { return false; }
  el.setAttribute("data-ah-value", json);
  kids(el, "input[type=hidden]").forEach((i) => { i.value = json; });
  if (fire) { emit(el, "change"); }
  return true;
}

// A component event with its payload in data-* attributes of the root,
// which the action receives as Event.data (the value is also the
// event's detail).
function fire(el, type, key, value) {
  el.setAttribute("data-" + key, value);
  emit(el, type, value);
}

// Follow one pointer press on the document: `start' after DISTANCE px
// (or at once with threshold 0; returning false gives up), then `move',
// then `end(ev, cancelled)' on release, pointercancel or Escape.
function track(owner, e, h, threshold) {
  const sx = e.clientX, sy = e.clientY;
  const min = threshold === undefined ? DISTANCE : threshold;
  const ac = new AbortController();
  const opts = { signal: ac.signal };
  let started = false;
  function stop() {
    ac.abort();
    document.documentElement.classList.remove("ah-dock-dragging");
    if (drags.get(owner) === cancel) { drags.delete(owner); }
  }
  function cancel() {
    stop();
    if (started) { h.end(null, true); }
  }
  function begin(ev) {
    started = true;
    if (h.start(ev) === false) { stop(); return false; }
    document.documentElement.classList.add("ah-dock-dragging");
    return true;
  }
  const prev = drags.get(owner);
  if (prev) { prev(); }
  drags.set(owner, cancel);
  if (min === 0 && !begin(e)) { return; }
  document.addEventListener("pointermove", (ev) => {
    if (!started) {
      if (Math.abs(ev.clientX - sx) < min && Math.abs(ev.clientY - sy) < min) { return; }
      if (!begin(ev)) { return; }
    }
    ev.preventDefault();
    h.move(ev);
  }, opts);
  const up = (ev) => {
    stop();
    if (started) { h.end(ev, ev.type === "pointercancel"); }
  };
  document.addEventListener("pointerup", up, opts);
  document.addEventListener("pointercancel", up, opts);
  document.addEventListener("keydown", (ev) => {
    if (ev.key === "Escape" && started) {
      ev.preventDefault();
      stop();
      h.end(ev, true);
    }
  }, opts);
}

function stopDrag(el) {
  const stop = drags.get(el);
  if (stop) { stop(); }
}

function own(el, node, rootSel) { return node.closest(rootSel) === el; }

// Parse trusted HTML (a server render or a shared template) into nodes.
function parseHTML(html) {
  const t = document.createElement("template");
  t.innerHTML = String(html).trim();
  return Array.from(t.content.children);
}

// Remove an element the way the runtime does (behaviours destroyed first).
function drop(node) {
  if (!node) { return; }
  AH.destroy(node);
  node.remove();
}

L.inside = inside;
L.parse = parse;
L.emit = emit;
L.kids = kids;
L.commit = commit;
L.fire = fire;
L.track = track;
L.stopDrag = stopDrag;
L.own = own;
L.parseHTML = parseHTML;
L.drop = drop;
L.next = function () { return ++L.seq; };
