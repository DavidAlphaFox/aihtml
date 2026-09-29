/* The dragdrop behaviour (designs/04-components.md), ported from sigil's
 * layout/dragdrop: [data-ah-drag] items dropped on [data-ah-drop] zones
 * fire ah:drop on the zone; the root carries data-drag, data-drop and
 * data-from for the action payload. The drag machinery is shared with
 * sortable.js (_lib_dnd.js).
 *
 * Events (native, bubbling): on the item ah:drag-start (detail {key}),
 * ah:dragging (detail {pageX, pageY}), ah:drag-end, ah:drag-cancel; on
 * the zone ah:drop-target-enter, ah:drop-target-leave and ah:drop
 * (detail {drag, drop, from}).
 */
import AH from "../core.js";
import "./_lib_dnd.js";

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
// dragdrop
// ==================================================================

function ddScope(el, node) {
  return node.closest("[data-ah=dragdrop]") === el;
}

function ddUsable(el, item) {
  return !el.classList.contains("ah-dragdrop-disabled") &&
    !item.classList.contains("ah-draggable-disabled");
}

// The zones of this scope that take `item' (its data-ah-drag-type must
// be in a zone's data-ah-drop-accept, when the zone has one).
function ddZones(el, item) {
  var type = item.getAttribute("data-ah-drag-type") || "";
  return Array.from(el.querySelectorAll("[data-ah-drop]")).filter(function (z) {
    if (!ddScope(el, z) || z.getAttribute("data-ah-drop-disabled") === "true" ||
        item.contains(z)) {
      return false;
    }
    var accept = z.getAttribute("data-ah-drop-accept");
    return !accept || accept.split(",").indexOf(type) >= 0;
  });
}

// sigil's hit-test: the last zone (in document order, so an inner zone
// beats its container) that the copy overlaps (intersect), lies in
// (fit), or that the pointer is on (pointer).
function ddHit(d, x, y) {
  var tol = d.el.getAttribute("data-ah-tolerance") || "intersect";
  var f = pageRect(d.copy), hit = null;
  d.zones.forEach(function (z) {
    var r = pageRect(z);
    var ok = tol === "pointer" ? inside(x, y, r)
      : tol === "fit" ? f.left >= r.left && f.right <= r.right && f.top >= r.top && f.bottom <= r.bottom
      : f.left < r.right && f.right > r.left && f.top < r.bottom && f.bottom > r.top;
    if (ok) { hit = z; }
  });
  return hit;
}

function ddTarget(d, zone) {
  if (zone === d.target) { return; }
  if (d.target) {
    d.target.classList.remove("ah-drop-target-active");
    fire(d.target, "ah:drop-target-leave");
  }
  d.target = zone;
  if (zone) {
    zone.classList.add("ah-drop-target-active");
    fire(zone, "ah:drop-target-enter");
  }
}

function ddBegin(d) {
  var item = d.item;
  d.zones = ddZones(d.el, item);
  var from = item.parentElement && item.parentElement.closest("[data-ah-drop]");
  d.from = from && ddScope(d.el, from) ? from.getAttribute("data-ah-drop") : "";
  d.target = null;
  item.classList.add("ah-dragging");
  d.zones.forEach(function (z) { z.classList.add("ah-drop-zone-accepting"); });
  d.el.classList.add("ah-dragdrop-active");
  fire(item, "ah:drag-start", { key: item.getAttribute("data-ah-drag") });
}

function ddFinish(d, dropped) {
  var item = d.item;
  item.classList.remove("ah-dragging");
  d.zones.forEach(function (z) { z.classList.remove("ah-drop-zone-accepting", "ah-drop-target-active"); });
  d.el.classList.remove("ah-dragdrop-active");
  document.body.classList.remove("ah-disableselect");
  var copy = d.copy;
  if (copy) {
    if (!dropped && d.el.hasAttribute("data-ah-revert") && d.orig) {
      copy.style.transition = "left .2s ease, top .2s ease";
      copy.style.left = d.orig.left + "px";
      copy.style.top = d.orig.top + "px";
      setTimeout(function () { copy.remove(); }, 220);
    } else {
      copy.remove();
    }
  }
  fire(item, "ah:drag-end");
}

function ddDrop(d) {
  var zone = d.target, item = d.item, el = d.el;
  var key = item.getAttribute("data-ah-drag");
  var dest = zone.getAttribute("data-ah-drop");
  var focused = document.activeElement === item;
  zone.classList.remove("ah-drop-target-active");
  if (el.hasAttribute("data-ah-move") && item.parentNode !== zone) {
    zone.appendChild(item);
    if (focused) { item.focus(); }
  }
  ddFinish(d, true);
  el.setAttribute("data-drag", key);
  el.setAttribute("data-drop", dest);
  el.setAttribute("data-from", d.from);
  fire(zone, "ah:drop", { drag: key, drop: dest, from: d.from });
  announce(ddLive(el), label(item) + " dropped on " + label(zone) + ".");
}

function ddCancel(d) {
  ddTarget(d, null);
  ddFinish(d, false);
  fire(d.item, "ah:drag-cancel");
}

function ddLive(el) {
  var live = el.querySelector(":scope > .ah-dnd-live");
  if (!live) {
    live = document.createElement("span");
    live.className = "ah-sortable-live ah-dnd-live";
    live.setAttribute("aria-live", "assertive");
    live.setAttribute("aria-atomic", "true");
    el.appendChild(live);
  }
  return live;
}

// Keyboard: Space/Enter picks up, arrows walk the zones, Space/Enter
// drops, Escape (or leaving the item) cancels.
function ddKeydown(el, item, e) {
  var d = L.drag && L.drag.kind === "dragdrop-key" && L.drag.item === item ? L.drag : null;
  var pick = e.key === " " || e.key === "Enter";
  if (!d) {
    if (pick && !L.drag && ddUsable(el, item)) {
      e.preventDefault();
      d = { kind: "dragdrop-key", el: el, item: item };
      ddBegin(d);
      if (!d.zones.length) {
        ddFinish(d, false);
        announce(ddLive(el), "No drop zone takes " + label(item) + ".");
        return;
      }
      L.drag = d;
      d.cancel = function () { L.drag = null; ddCancel(d); };
      d.pos = -1;
      announce(ddLive(el), "Picked up " + label(item) + ". Arrow keys choose one of " +
               d.zones.length + " drop zones, Space drops, Escape cancels.");
    }
    return;
  }
  var n = d.zones.length;
  if (/^Arrow/.test(e.key)) {
    e.preventDefault();
    var step = e.key === "ArrowUp" || e.key === "ArrowLeft" ? -1 : 1;
    d.pos = d.pos < 0 ? (step > 0 ? 0 : n - 1) : (d.pos + step + n) % n;
    ddTarget(d, d.zones[d.pos]);
    announce(ddLive(el), label(d.zones[d.pos]) + ", drop zone " + (d.pos + 1) + " of " + n + ".");
  } else if (pick) {
    e.preventDefault();
    L.drag = null;
    if (d.target) { ddDrop(d); } else { ddCancel(d); }
  } else if (e.key === "Escape") {
    e.preventDefault();
    d.cancel();
    announce(ddLive(el), "Cancelled.");
  }
}

AH.register("dragdrop", class extends AH.Controller {
  setup() {
    var el = this.element;
    this.delegate("pointerdown", "[data-ah-drag]", function (e, item) {
      if (L.drag || !ddScope(el, item) || !ddUsable(el, item) ||
          (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
        return;
      }
      e.preventDefault();
      try { item.focus({ preventScroll: true }); } catch (err) { /* ignore */ }
      var d = { kind: "dragdrop", el: el, item: item, pointerId: e.pointerId,
                x0: px(e), y0: py(e), started: false };
      track(d, function (me) {
        if (!d.started) {
          if (Math.abs(px(me) - d.x0) + Math.abs(py(me) - d.y0) <= DISTANCE) { return; }
          d.started = true;
          var r = pageRect(item);
          d.orig = { left: r.left, top: r.top };
          d.offX = d.x0 - r.left;
          d.offY = d.y0 - r.top;
          d.box = scrollParent(el);
          d.copy = floatingCopy(item, "ah-drag-feedback", 0.6);
          document.body.classList.add("ah-disableselect");
          ddBegin(d);
        }
        d.px = px(me); d.py = py(me); d.cx = me.clientX; d.cy = me.clientY;
        if (d.raf) { return; }
        d.raf = requestAnimationFrame(function () {
          d.raf = 0;
          if (L.drag !== d) { return; }
          d.copy.style.left = (d.px - d.offX) + "px";
          d.copy.style.top = (d.py - d.offY) + "px";
          autoScroll(d.box, d.cx, d.cy);
          ddTarget(d, ddHit(d, d.px, d.py));
          fire(item, "ah:dragging", { pageX: d.px, pageY: d.py });
        });
      }, function (ue) {
        if (!d.started) { return; }
        swallowClick();
        // the last frame may not have run: test where the pointer let go
        d.copy.style.left = (px(ue) - d.offX) + "px";
        d.copy.style.top = (py(ue) - d.offY) + "px";
        ddTarget(d, ddHit(d, px(ue), py(ue)));
        if (d.target) { ddDrop(d); } else { ddFinish(d, false); }
      }, function () {
        if (d.started) { ddCancel(d); }
      });
    });
    this.delegate("keydown", "[data-ah-drag]", function (e, item) {
      if (e.target === item && ddScope(el, item)) { ddKeydown(el, item, e); }
    });
    this.delegate("focusout", "[data-ah-drag]", function (e, item) {
      setTimeout(function () {
        if (L.drag && L.drag.kind === "dragdrop-key" && L.drag.item === item &&
            document.activeElement !== item) {
          L.drag.cancel();
        }
      }, 0);
    });
  }

  teardown() {
    cancelFor(this.element);
    var live = this.element.querySelector(":scope > .ah-dnd-live");
    if (live) { live.remove(); }
  }

  // methods (aihtml_action:call/4, AH.invoke)
  enable() {
    this.element.classList.remove("ah-dragdrop-disabled");
    this.element.removeAttribute("aria-disabled");
  }

  disable() {
    cancelFor(this.element);
    this.element.classList.add("ah-dragdrop-disabled");
    this.element.setAttribute("aria-disabled", "true");
  }

  cancel() { cancelFor(this.element); }
});
