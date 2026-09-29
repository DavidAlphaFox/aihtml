/* The sortable behaviour (designs/04-components.md), ported from sigil's
 * layout/sortable (+ sortable/geometry): reorder by dragging (or from the
 * keyboard); the value is the order of the item keys (data-value),
 * change after a drop that changed it; lists with the same data-ah-group
 * exchange items. The drag machinery is shared with dragdrop.js
 * (_lib_dnd.js).
 *
 * Events (native, bubbling) on the lists: ah:sort-start and ah:sort-stop
 * (detail {key, index}), ah:sort-change, ah:sort-remove, ah:sort-receive,
 * ah:sort-cancel (detail {key}), change.
 */
import AH from "../core.js";
import "./_lib_dnd.js";
import "./_lib_values.js";

var L = AH.lib.dnd;
var DISTANCE = L.DISTANCE;
var pageRect = L.pageRect,
    px = L.px,
    py = L.py,
    inside = L.inside,
    floatingCopy = L.floatingCopy,
    scrollParent = L.scrollParent,
    autoScroll = L.autoScroll,
    swallowClick = L.swallowClick,
    editable = L.editable,
    announce = L.announce,
    label = L.label,
    fire = L.fire,
    track = L.track,
    cancelFor = L.cancelFor;

// ==================================================================
// sortable
// ==================================================================

function soItems(list) {
  return Array.from(list.querySelectorAll(":scope > .ah-sortable-item"));
}

function soKey(item) { return item.getAttribute("data-value") || ""; }

function soOrder(list) {
  return AH.lib.values.join(soItems(list).map(soKey));
}

function soDisabled(list) {
  return list.classList.contains("ah-sortable-disabled");
}

function soLive(list) { return list.querySelector(":scope > .ah-sortable-live"); }

function soPublish(list, notify) {
  var v = soOrder(list);
  var old = list.getAttribute("data-ah-value") || "";
  list.setAttribute("data-ah-value", v);
  var hidden = list.querySelector(":scope > input[type=hidden]");
  if (hidden) { hidden.value = v; }
  if (notify && v !== old) { fire(list, "change"); }
}

// Roving tabindex: `item' (or the first item) is the one in the tab order.
function soRove(list, item) {
  var items = soItems(list);
  if (!item || items.indexOf(item) < 0) {
    item = items.filter(function (it) { return it.getAttribute("tabindex") === "0"; })[0] || items[0];
  }
  var off = soDisabled(list);
  items.forEach(function (it) { it.setAttribute("tabindex", "-1"); });
  if (item && !off) { item.setAttribute("tabindex", "0"); }
}

function soLayout(list) {
  return list.classList.contains("ah-sortable-grid") ? "grid"
    : list.classList.contains("ah-sortable-horizontal") ? "horizontal" : "vertical";
}

// Where the placeholder goes for the pointer at (x, y): the item to insert
// before, or null for the end. Vertical lists compare with the middle
// of each item; horizontal lists and grids go in reading order (sigil's
// find-grid-insertion), which also handles wrapped rows.
function soInsertion(items, layout, x, y) {
  for (var i = 0; i < items.length; i++) {
    var r = pageRect(items[i]);
    if (layout === "vertical") {
      if (y < r.top + r.height / 2) { return items[i]; }
    } else if (y < r.top || (y <= r.bottom && x < r.left + r.width / 2)) {
      return items[i];
    }
  }
  return null;
}

// Put `node' before `ref', or after the last item of the list.
function soPlace(list, node, ref) {
  if (ref) {
    if (ref.previousSibling !== node) { list.insertBefore(node, ref); }
    return;
  }
  var items = Array.from(list.querySelectorAll(":scope > .ah-sortable-item, :scope > .ah-sortable-placeholder"))
    .filter(function (n) { return n !== node && n.style.display !== "none"; });
  var last = items[items.length - 1];
  var after = last ? last.nextSibling : list.firstChild;
  if (after !== node) { list.insertBefore(node, after); }
}

// Connected lists under the pointer: the innermost (smallest) one wins,
// as in sigil's check-connected-containers!.
function soTarget(d, x, y) {
  var group = d.el.getAttribute("data-ah-group");
  if (!group) { return d.list; }
  var best = null, area = Infinity;
  document.querySelectorAll(".ah-sortable[data-ah-group]").forEach(function (l) {
    if (l.getAttribute("data-ah-group") !== group || (soDisabled(l) && l !== d.el)) {
      return;
    }
    var r = pageRect(l);
    if (inside(x, y, r) && r.width * r.height < area) { best = l; area = r.width * r.height; }
  });
  return best || d.list;
}

function soStart(d) {
  var item = d.item;
  var r = pageRect(item);
  var cs = window.getComputedStyle(item);
  var ph = document.createElement(item.tagName);
  ph.className = "ah-sortable-placeholder";
  Object.assign(ph.style, { width: r.width + "px", height: r.height + "px", margin: cs.margin,
                            flex: "none" });
  d.helper = floatingCopy(item, "ah-sortable-helper", 0.85);
  d.offX = d.x0 - r.left;
  d.offY = d.y0 - r.top;
  d.ph = ph;
  d.index = soItems(d.el).indexOf(item);
  d.next = item.nextSibling;
  d.list = d.el;
  d.box = scrollParent(d.el);
  d.started = true;
  item.parentNode.insertBefore(ph, item.nextSibling);
  d.display = item.style.display;
  item.style.display = "none";
  d.el.classList.add("ah-sortable-active");
  document.body.classList.add("ah-disableselect");
  fire(d.el, "ah:sort-start", { key: soKey(item), index: d.index });
}

function soMove(d, e) {
  d.px = px(e); d.py = py(e); d.cx = e.clientX; d.cy = e.clientY;
  if (d.raf) { return; }
  d.raf = requestAnimationFrame(function () {
    d.raf = 0;
    if (L.drag !== d) { return; }
    d.helper.style.left = (d.px - d.offX) + "px";
    d.helper.style.top = (d.py - d.offY) + "px";
    autoScroll(d.box, d.cx, d.cy);
    var target = soTarget(d, d.px, d.py);
    if (target !== d.list) {
      d.list.classList.remove("ah-sortable-receiving");
      if (d.list !== d.el) { d.list.classList.remove("ah-sortable-active"); }
      fire(d.list, "ah:sort-remove", { key: soKey(d.item) });
      d.list = target;
      if (target !== d.el) { target.classList.add("ah-sortable-receiving", "ah-sortable-active"); }
      fire(target, "ah:sort-receive", { key: soKey(d.item) });
    }
    var items = soItems(d.list).filter(function (n) { return n !== d.item; });
    var ref = soInsertion(items, soLayout(d.list), d.px, d.py);
    var before = d.ph.nextSibling, parent = d.ph.parentNode;
    soPlace(d.list, d.ph, ref);
    if (d.ph.nextSibling !== before || d.ph.parentNode !== parent) {
      fire(d.list, "ah:sort-change", { key: soKey(d.item) });
    }
  });
}

function soCleanup(d) {
  d.item.style.display = d.display;
  d.ph.remove();
  d.helper.remove();
  [d.el, d.list].forEach(function (l) { l.classList.remove("ah-sortable-active", "ah-sortable-receiving"); });
  document.body.classList.remove("ah-disableselect");
}

function soEnd(d) {
  var item = d.item, from = d.el, to = d.list;
  to.insertBefore(item, d.ph);
  soCleanup(d);
  swallowClick();
  soRove(to, item);
  if (from !== to) { soRove(from, null); }
  var index = soItems(to).indexOf(item);
  fire(to, "ah:sort-stop", { key: soKey(item), index: index });
  soPublish(from, true);
  if (from !== to) { soPublish(to, true); }
  try { item.focus({ preventScroll: true }); } catch (err) { /* detached */ }
}

function soCancel(d) {
  if (!d.started) { return; }
  soCleanup(d);
  fire(d.el, "ah:sort-cancel", { key: soKey(d.item) });
}

function soPos(list, item) {
  var items = soItems(list);
  return (items.indexOf(item) + 1) + " of " + items.length;
}

function sibling(item, dir) {
  var n = dir < 0 ? item.previousElementSibling : item.nextElementSibling;
  while (n && !n.classList.contains("ah-sortable-item")) {
    n = dir < 0 ? n.previousElementSibling : n.nextElementSibling;
  }
  return n;
}

// Move `item' by `delta' places (or to the start / end for -/+Infinity)
// by moving its neighbours, so the item itself keeps the focus.
function soShift(list, item, delta) {
  var moved = false;
  while (delta < 0) {
    var prev = sibling(item, -1);
    if (!prev) { break; }
    list.insertBefore(prev, item.nextSibling);
    moved = true; delta++;
  }
  while (delta > 0) {
    var next = sibling(item, 1);
    if (!next) { break; }
    list.insertBefore(next, item);
    moved = true; delta--;
  }
  return moved;
}

function soSetOrder(list, keys) {
  var items = soItems(list);
  var byKey = {};
  items.forEach(function (it) { byKey[soKey(it)] = it; });
  var named = [];
  keys.forEach(function (k) {
    if (byKey[k] && named.indexOf(byKey[k]) < 0) { named.push(byKey[k]); }
  });
  var rest = items.filter(function (it) { return named.indexOf(it) < 0; });
  var anchor = items[items.length - 1];
  anchor = anchor ? anchor.nextSibling : list.firstChild;
  named.concat(rest).forEach(function (it) {
    if (it !== anchor) { list.insertBefore(it, anchor); } else { anchor = it.nextSibling; }
  });
}

function soKeys(layout) {
  return layout === "vertical" ? { prev: ["ArrowUp"], next: ["ArrowDown"] }
    : layout === "horizontal" ? { prev: ["ArrowLeft"], next: ["ArrowRight"] }
    : { prev: ["ArrowLeft", "ArrowUp"], next: ["ArrowRight", "ArrowDown"] };
}

AH.register("sortable", class extends AH.Controller {
  setup() {
    var el = this.element;
    // keyboard: the picked-up item and the order before it moved
    this.grabbed = null;
    this.order = null;
    soRove(el, null);
    this.delegate("pointerdown", ".ah-sortable-item", (e, item) => {
      if (item.parentNode !== el || L.drag || soDisabled(el) ||
          (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
        return;
      }
      if (item.classList.contains("ah-sortable-handle-mode") &&
          !e.target.closest(".ah-sortable-handle")) {
        return;
      }
      e.preventDefault();
      this.release(true);
      soRove(el, item);
      try { item.focus({ preventScroll: true }); } catch (err) { /* ignore */ }
      var d = { kind: "sortable", el: el, item: item, pointerId: e.pointerId,
                x0: px(e), y0: py(e), started: false };
      track(d, function (me) {
        if (!d.started) {
          if (Math.abs(px(me) - d.x0) + Math.abs(py(me) - d.y0) <= DISTANCE) { return; }
          soStart(d);
        }
        soMove(d, me);
      }, function () {
        if (d.started) { soEnd(d); }
      }, function () {
        soCancel(d);
      });
    });
    this.delegate("keydown", ".ah-sortable-item", (e, item) => {
      if (e.target === item && item.parentNode === el) { this.keydown(item, e); }
    });
    this.delegate("focusout", ".ah-sortable-item", (e, item) => {
      // a Tab away (or a click elsewhere) drops the picked-up item
      setTimeout(() => {
        if (this.grabbed === item && document.activeElement !== item) {
          this.release(true);
        }
      }, 0);
    });
    this.delegate("focusin", ".ah-sortable-item", (e, item) => {
      if (item.parentNode === el && !soDisabled(el)) { soRove(el, item); }
    });
  }

  teardown() {
    cancelFor(this.element);
    this.grabbed = null;
  }

  // methods (aihtml_action:call/4, AH.invoke)
  getValue() { return soOrder(this.element); }

  setValue(order) {
    var keys = AH.lib.values.split(Array.isArray(order) ? order : String(order || ""))
      .filter(Boolean);
    soSetOrder(this.element, keys);
    soPublish(this.element, false);
  }

  enable() {
    this.element.classList.remove("ah-sortable-disabled");
    this.element.removeAttribute("aria-disabled");
    soRove(this.element, null);
  }

  disable() {
    cancelFor(this.element);
    this.release(true);
    this.element.classList.add("ah-sortable-disabled");
    this.element.setAttribute("aria-disabled", "true");
    soRove(this.element, null);
  }

  cancel() {
    cancelFor(this.element);
    if (this.grabbed) { this.release(false); }
  }

  // ---- keyboard: a picked-up item moves with the arrows ----

  grab(item) {
    var list = this.element;
    this.grabbed = item;
    this.order = soOrder(list);
    item.classList.add("ah-sortable-item-grabbed");
    item.setAttribute("aria-pressed", "true");
    announce(soLive(list), "Picked up " + label(item) + ", position " + soPos(list, item) +
             ". Arrow keys move it, Space drops it, Escape cancels.");
  }

  release(commit) {
    var list = this.element, item = this.grabbed;
    if (!item) { return; }
    this.grabbed = null;
    item.classList.remove("ah-sortable-item-grabbed");
    item.removeAttribute("aria-pressed");
    var live = soLive(list);
    if (commit) {
      announce(live, label(item) + " dropped at position " + soPos(list, item) + ".");
      soPublish(list, true);
    } else {
      soSetOrder(list, AH.lib.values.split(this.order));
      item.focus();
      announce(live, "Cancelled, " + label(item) + " is back at position " + soPos(list, item) + ".");
    }
  }

  keydown(item, e) {
    var list = this.element;
    var keys = soKeys(soLayout(list));
    var dir = keys.prev.indexOf(e.key) >= 0 ? -1 : keys.next.indexOf(e.key) >= 0 ? 1
      : e.key === "Home" ? -Infinity : e.key === "End" ? Infinity : 0;
    var off = soDisabled(list);
    if (this.grabbed === item) {
      if (dir) {
        e.preventDefault();
        if (soShift(list, item, dir)) {
          announce(soLive(list), label(item) + ", position " + soPos(list, item) + ".");
        }
      } else if (e.key === " " || e.key === "Enter") {
        e.preventDefault();
        this.release(true);
      } else if (e.key === "Escape") {
        e.preventDefault();
        this.release(false);
      }
      return;
    }
    if (dir && e.altKey && !off) {
      e.preventDefault();
      if (soShift(list, item, dir)) { soPublish(list, true); }
      return;
    }
    if (dir) {
      e.preventDefault();
      var items = soItems(list), i = items.indexOf(item);
      var j = dir === -Infinity ? 0 : dir === Infinity ? items.length - 1
        : Math.max(0, Math.min(items.length - 1, i + dir));
      soRove(list, items[j]);
      items[j].focus();
    } else if ((e.key === " " || e.key === "Enter") && !off) {
      e.preventDefault();
      this.grab(item);
    }
  }
});
