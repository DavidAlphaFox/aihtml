/* Behaviours of the layout_dnd components (designs/04-components.md).
 * Ported from sigil: layout/sortable (+ sortable/geometry) and
 * layout/dragdrop. Pointer events (mouse, pen, touch) instead of sigil's
 * mouse + touch sequence; one drag at a time, tracked on the document
 * with its own namespace and unbound when the drag ends or the
 * component is destroyed.
 *
 *   sortable   reorder by dragging (or from the keyboard); the value is
 *              the order of the item keys (data-value), change after a
 *              drop that changed it; lists with the same data-ah-group
 *              exchange items
 *   dragdrop   [data-ah-drag] items dropped on [data-ah-drop] zones fire
 *              ah:drop on the zone; the root carries data-drag, data-drop
 *              and data-from for the action payload
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
  var DOC_NS = ".ahdnd";
  var DISTANCE = 5;            // px of movement before a press becomes a drag
  var EDGE = 20, SPEED = 10;   // auto scroll: edge width and px per frame

  // ------------------------------------------------------------------
  // Shared
  // ------------------------------------------------------------------

  // The drag in progress (one per page): {kind, el, pointerId, ...}
  var drag = null;

  function pageRect(el) {
    var r = el.getBoundingClientRect();
    var sx = window.pageXOffset, sy = window.pageYOffset;
    return { left: r.left + sx, top: r.top + sy, right: r.right + sx, bottom: r.bottom + sy,
             width: r.width, height: r.height };
  }

  // Page coordinates of a pointer event (from clientX, which every
  // pointer event has, synthetic ones included).
  function px(e) { return e.clientX + window.pageXOffset; }
  function py(e) { return e.clientY + window.pageYOffset; }

  function inside(x, y, r) {
    return x >= r.left && x <= r.right && y >= r.top && y <= r.bottom;
  }

  // The copy that follows the pointer lives in <body>, outside the scope of
  // the custom properties it inherited (skins, a themed container), so they
  // are copied onto it, as sigil does.
  function copyVars(src, dst) {
    var cs = window.getComputedStyle(src);
    for (var i = 0; i < cs.length; i++) {
      var p = cs.item(i);
      if (p && p.indexOf("--") === 0) { dst.style.setProperty(p, cs.getPropertyValue(p)); }
    }
  }

  function floatingCopy(el, cls, opacity) {
    var r = pageRect(el);
    var copy = el.cloneNode(true);
    copy.removeAttribute("id");
    $(copy).find("[id]").removeAttr("id");
    copy.removeAttribute("tabindex");
    copy.setAttribute("aria-hidden", "true");
    copy.className += " " + cls;
    copyVars(el, copy);
    $(copy).css({ position: "absolute", margin: 0, boxSizing: "border-box",
                  width: r.width + "px", height: r.height + "px",
                  left: r.left + "px", top: r.top + "px", opacity: opacity,
                  zIndex: 999999, pointerEvents: "none" });
    document.body.appendChild(copy);
    return copy;
  }

  function scrollParent(el) {
    for (var cur = el.parentElement; cur && cur !== document.body; cur = cur.parentElement) {
      var s = window.getComputedStyle(cur);
      if ((/auto|scroll/.test(s.overflowY) && cur.scrollHeight > cur.clientHeight) ||
          (/auto|scroll/.test(s.overflowX) && cur.scrollWidth > cur.clientWidth)) {
        return cur;
      }
    }
    return null;
  }

  // Scroll the nearest scrollable ancestor (or the window) when the
  // pointer is near its edge.
  function autoScroll(box, cx, cy) {
    if (box) {
      var r = box.getBoundingClientRect();
      if (cy - r.top < EDGE) { box.scrollTop -= SPEED; }
      else if (r.bottom - cy < EDGE) { box.scrollTop += SPEED; }
      if (cx - r.left < EDGE) { box.scrollLeft -= SPEED; }
      else if (r.right - cx < EDGE) { box.scrollLeft += SPEED; }
    }
    if (cy < EDGE) { window.scrollBy(0, -SPEED); }
    else if (window.innerHeight - cy < EDGE) { window.scrollBy(0, SPEED); }
  }

  // Swallow the click that follows a drag (items may be links).
  function swallowClick() {
    var stop = function (e) { e.stopPropagation(); e.preventDefault(); };
    document.addEventListener("click", stop, true);
    setTimeout(function () { document.removeEventListener("click", stop, true); }, 0);
  }

  function editable(t) {
    return $(t).closest("input, textarea, select, [contenteditable=''], [contenteditable=true]").length > 0;
  }

  function announce($live, text) {
    $live.text("");
    setTimeout(function () { $live.text(text); }, 20);
  }

  function label(el) {
    return el.getAttribute("aria-label") || $(el).text().trim().replace(/\s+/g, " ").slice(0, 60);
  }

  // Document listeners for one drag; `move', `end' and `cancel' get the
  // pointer event (or nothing for a cancel from the keyboard).
  function track(d, move, end, cancel) {
    drag = d;
    var $doc = $(document);
    $doc.on("pointermove" + DOC_NS, function (e) {
      if (e.pointerId === d.pointerId) { move(e); }
    });
    $doc.on("pointerup" + DOC_NS, function (e) {
      if (e.pointerId === d.pointerId) { untrack(); end(e); }
    });
    $doc.on("pointercancel" + DOC_NS, function (e) {
      if (e.pointerId === d.pointerId) { untrack(); cancel(); }
    });
    $doc.on("keydown" + DOC_NS, function (e) {
      if (e.key === "Escape") { e.preventDefault(); untrack(); cancel(); }
    });
    d.cancel = function () { untrack(); cancel(); };
  }

  function untrack() {
    $(document).off(DOC_NS);
    if (drag && drag.raf) { cancelAnimationFrame(drag.raf); }
    drag = null;
  }

  // Cancel the drag in progress if it belongs to el.
  function cancelFor(el) {
    if (drag && drag.el === el) { drag.cancel(); }
  }

  // ==================================================================
  // sortable
  // ==================================================================

  function soItems(list) {
    return $(list).children(".ah-sortable-item").get();
  }

  function soKey(item) { return item.getAttribute("data-value") || ""; }

  function soOrder(list) {
    return soItems(list).map(soKey).join(",");
  }

  function soDisabled(list) {
    return $(list).hasClass("ah-sortable-disabled");
  }

  function soPublish(list, fire) {
    var v = soOrder(list);
    var old = list.getAttribute("data-ah-value") || "";
    list.setAttribute("data-ah-value", v);
    $(list).children("input[type=hidden]").val(v);
    if (fire && v !== old) { $(list).trigger("change"); }
  }

  // Roving tabindex: `item' (or the first item) is the one in the tab order.
  function soRove(list, item) {
    var items = soItems(list);
    if (!item || items.indexOf(item) < 0) {
      item = $(items).filter("[tabindex=0]")[0] || items[0];
    }
    var off = soDisabled(list);
    $(items).attr("tabindex", "-1");
    if (item && !off) { item.setAttribute("tabindex", "0"); }
  }

  function soLayout(list) {
    var $l = $(list);
    return $l.hasClass("ah-sortable-grid") ? "grid"
      : $l.hasClass("ah-sortable-horizontal") ? "horizontal" : "vertical";
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
    var items = $(list).children(".ah-sortable-item, .ah-sortable-placeholder").get()
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
    $(".ah-sortable[data-ah-group]").each(function () {
      if (this.getAttribute("data-ah-group") !== group || (soDisabled(this) && this !== d.el)) {
        return;
      }
      var r = pageRect(this);
      if (inside(x, y, r) && r.width * r.height < area) { best = this; area = r.width * r.height; }
    });
    return best || d.list;
  }

  function soStart(d) {
    var item = d.item;
    var r = pageRect(item);
    var cs = window.getComputedStyle(item);
    var ph = document.createElement(item.tagName);
    ph.className = "ah-sortable-placeholder";
    $(ph).css({ width: r.width + "px", height: r.height + "px", margin: cs.margin,
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
    $(d.el).addClass("ah-sortable-active");
    $("body").addClass("ah-disableselect");
    $(d.el).trigger("ah:sort-start", [{ key: soKey(item), index: d.index }]);
  }

  function soMove(d, e) {
    d.px = px(e); d.py = py(e); d.cx = e.clientX; d.cy = e.clientY;
    if (d.raf) { return; }
    d.raf = requestAnimationFrame(function () {
      d.raf = 0;
      if (drag !== d) { return; }
      $(d.helper).css({ left: (d.px - d.offX) + "px", top: (d.py - d.offY) + "px" });
      autoScroll(d.box, d.cx, d.cy);
      var target = soTarget(d, d.px, d.py);
      if (target !== d.list) {
        $(d.list).removeClass("ah-sortable-receiving");
        if (d.list !== d.el) { $(d.list).removeClass("ah-sortable-active"); }
        $(d.list).trigger("ah:sort-remove", [{ key: soKey(d.item) }]);
        d.list = target;
        if (target !== d.el) { $(target).addClass("ah-sortable-receiving ah-sortable-active"); }
        $(target).trigger("ah:sort-receive", [{ key: soKey(d.item) }]);
      }
      var items = soItems(d.list).filter(function (n) { return n !== d.item; });
      var ref = soInsertion(items, soLayout(d.list), d.px, d.py);
      var before = d.ph.nextSibling, parent = d.ph.parentNode;
      soPlace(d.list, d.ph, ref);
      if (d.ph.nextSibling !== before || d.ph.parentNode !== parent) {
        $(d.list).trigger("ah:sort-change", [{ key: soKey(d.item) }]);
      }
    });
  }

  function soCleanup(d) {
    d.item.style.display = d.display;
    $(d.ph).remove();
    $(d.helper).remove();
    $(d.el).add(d.list).removeClass("ah-sortable-active ah-sortable-receiving");
    $("body").removeClass("ah-disableselect");
  }

  function soEnd(d) {
    var item = d.item, from = d.el, to = d.list;
    to.insertBefore(item, d.ph);
    soCleanup(d);
    swallowClick();
    soRove(to, item);
    if (from !== to) { soRove(from, null); }
    var index = soItems(to).indexOf(item);
    $(to).trigger("ah:sort-stop", [{ key: soKey(item), index: index }]);
    soPublish(from, true);
    if (from !== to) { soPublish(to, true); }
    try { item.focus({ preventScroll: true }); } catch (err) { /* detached */ }
  }

  function soCancel(d) {
    if (!d.started) { return; }
    soCleanup(d);
    $(d.el).trigger("ah:sort-cancel", [{ key: soKey(d.item) }]);
  }

  // ---- keyboard: a picked-up item moves with the arrows ----

  function soState(list) {
    var st = $.data(list, "ahSortable");
    if (!st) { st = { grabbed: null, order: null }; $.data(list, "ahSortable", st); }
    return st;
  }

  function soPos(list, item) {
    var items = soItems(list);
    return (items.indexOf(item) + 1) + " of " + items.length;
  }

  // Move `item' by `delta' places (or to the start / end for -/+Infinity)
  // by moving its neighbours, so the item itself keeps the focus.
  function soShift(list, item, delta) {
    var moved = false;
    while (delta < 0) {
      var prev = $(item).prevAll(".ah-sortable-item")[0];
      if (!prev) { break; }
      list.insertBefore(prev, item.nextSibling);
      moved = true; delta++;
    }
    while (delta > 0) {
      var next = $(item).nextAll(".ah-sortable-item")[0];
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
    var anchor = $(list).children(".ah-sortable-item").last()[0];
    anchor = anchor ? anchor.nextSibling : list.firstChild;
    named.concat(rest).forEach(function (it) {
      if (it !== anchor) { list.insertBefore(it, anchor); } else { anchor = it.nextSibling; }
    });
  }

  function soGrab(list, item) {
    var st = soState(list);
    st.grabbed = item;
    st.order = soOrder(list);
    $(item).addClass("ah-sortable-item-grabbed").attr("aria-pressed", "true");
    announce($(list).children(".ah-sortable-live"),
             "Picked up " + label(item) + ", position " + soPos(list, item) +
             ". Arrow keys move it, Space drops it, Escape cancels.");
  }

  function soRelease(list, commit) {
    var st = soState(list), item = st.grabbed;
    if (!item) { return; }
    st.grabbed = null;
    $(item).removeClass("ah-sortable-item-grabbed").removeAttr("aria-pressed");
    var $live = $(list).children(".ah-sortable-live");
    if (commit) {
      announce($live, label(item) + " dropped at position " + soPos(list, item) + ".");
      soPublish(list, true);
    } else {
      soSetOrder(list, st.order.split(","));
      item.focus();
      announce($live, "Cancelled, " + label(item) + " is back at position " + soPos(list, item) + ".");
    }
  }

  function soKeys(layout) {
    return layout === "vertical" ? { prev: ["ArrowUp"], next: ["ArrowDown"] }
      : layout === "horizontal" ? { prev: ["ArrowLeft"], next: ["ArrowRight"] }
      : { prev: ["ArrowLeft", "ArrowUp"], next: ["ArrowRight", "ArrowDown"] };
  }

  function soKeydown(list, item, e) {
    var st = soState(list);
    var keys = soKeys(soLayout(list));
    var dir = keys.prev.indexOf(e.key) >= 0 ? -1 : keys.next.indexOf(e.key) >= 0 ? 1
      : e.key === "Home" ? -Infinity : e.key === "End" ? Infinity : 0;
    var off = soDisabled(list);
    if (st.grabbed === item) {
      if (dir) {
        e.preventDefault();
        if (soShift(list, item, dir)) {
          announce($(list).children(".ah-sortable-live"),
                   label(item) + ", position " + soPos(list, item) + ".");
        }
      } else if (e.key === " " || e.key === "Enter") {
        e.preventDefault();
        soRelease(list, true);
      } else if (e.key === "Escape") {
        e.preventDefault();
        soRelease(list, false);
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
      soGrab(list, item);
    }
  }

  AH.define("sortable", {
    init: function (el, $el) {
      soRove(el, null);
      $el.on("pointerdown" + NS, ".ah-sortable-item", function (e) {
        var item = this;
        if (item.parentNode !== el || drag || soDisabled(el) ||
            (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
          return;
        }
        if ($(item).hasClass("ah-sortable-handle-mode") &&
            !$(e.target).closest(".ah-sortable-handle").length) {
          return;
        }
        e.preventDefault();
        soRelease(el, true);
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
      $el.on("keydown" + NS, ".ah-sortable-item", function (e) {
        if (e.target === this && this.parentNode === el) { soKeydown(el, this, e); }
      });
      $el.on("focusout" + NS, ".ah-sortable-item", function () {
        var item = this;
        // a Tab away (or a click elsewhere) drops the picked-up item
        setTimeout(function () {
          if (soState(el).grabbed === item && document.activeElement !== item) {
            soRelease(el, true);
          }
        }, 0);
      });
      $el.on("focusin" + NS, ".ah-sortable-item", function () {
        if (this.parentNode === el && !soDisabled(el)) { soRove(el, this); }
      });
    },
    destroy: function (el) {
      cancelFor(el);
      $.removeData(el, "ahSortable");
    },
    methods: {
      getValue: function (el) { return soOrder(el); },
      setValue: function (el, $el, order) {
        var keys = Array.isArray(order) ? order.map(String)
          : String(order || "").split(",").filter(Boolean);
        soSetOrder(el, keys);
        soPublish(el, false);
      },
      enable: function (el, $el) {
        $el.removeClass("ah-sortable-disabled").removeAttr("aria-disabled");
        soRove(el, null);
      },
      disable: function (el, $el) {
        cancelFor(el);
        soRelease(el, true);
        $el.addClass("ah-sortable-disabled").attr("aria-disabled", "true");
        soRove(el, null);
      },
      cancel: function (el) {
        cancelFor(el);
        if (soState(el).grabbed) { soRelease(el, false); }
      }
    }
  });

  // ==================================================================
  // dragdrop
  // ==================================================================

  function ddScope(el, node) {
    return $(node).closest("[data-ah=dragdrop]")[0] === el;
  }

  function ddUsable(el, item) {
    return !$(el).hasClass("ah-dragdrop-disabled") && !$(item).hasClass("ah-draggable-disabled");
  }

  // The zones of this scope that take `item' (its data-ah-drag-type must
  // be in a zone's data-ah-drop-accept, when the zone has one).
  function ddZones(el, item) {
    var type = item.getAttribute("data-ah-drag-type") || "";
    return $(el).find("[data-ah-drop]").get().filter(function (z) {
      if (!ddScope(el, z) || z.getAttribute("data-ah-drop-disabled") === "true" ||
          $.contains(item, z) || z === item) {
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
      $(d.target).removeClass("ah-drop-target-active").trigger("ah:drop-target-leave");
    }
    d.target = zone;
    if (zone) {
      $(zone).addClass("ah-drop-target-active").trigger("ah:drop-target-enter");
    }
  }

  function ddBegin(d) {
    var item = d.item;
    d.zones = ddZones(d.el, item);
    var from = $(item).parent().closest("[data-ah-drop]")[0];
    d.from = from && ddScope(d.el, from) ? from.getAttribute("data-ah-drop") : "";
    d.target = null;
    $(item).addClass("ah-dragging");
    $(d.zones).addClass("ah-drop-zone-accepting");
    $(d.el).addClass("ah-dragdrop-active");
    $(item).trigger("ah:drag-start", [{ key: item.getAttribute("data-ah-drag") }]);
  }

  function ddFinish(d, dropped) {
    var item = d.item;
    $(item).removeClass("ah-dragging");
    $(d.zones).removeClass("ah-drop-zone-accepting ah-drop-target-active");
    $(d.el).removeClass("ah-dragdrop-active");
    $("body").removeClass("ah-disableselect");
    var copy = d.copy;
    if (copy) {
      if (!dropped && d.el.hasAttribute("data-ah-revert") && d.orig) {
        $(copy).css("transition", "left .2s ease, top .2s ease")
          .css({ left: d.orig.left + "px", top: d.orig.top + "px" });
        setTimeout(function () { $(copy).remove(); }, 220);
      } else {
        $(copy).remove();
      }
    }
    $(item).trigger("ah:drag-end");
  }

  function ddDrop(d) {
    var zone = d.target, item = d.item, el = d.el;
    var key = item.getAttribute("data-ah-drag");
    var dest = zone.getAttribute("data-ah-drop");
    var focused = document.activeElement === item;
    $(zone).removeClass("ah-drop-target-active");
    if (el.hasAttribute("data-ah-move") && item.parentNode !== zone) {
      zone.appendChild(item);
      if (focused) { item.focus(); }
    }
    ddFinish(d, true);
    el.setAttribute("data-drag", key);
    el.setAttribute("data-drop", dest);
    el.setAttribute("data-from", d.from);
    $(zone).trigger("ah:drop", [{ drag: key, drop: dest, from: d.from }]);
    announce(ddLive(el), label(item) + " dropped on " + label(zone) + ".");
  }

  function ddCancel(d) {
    ddTarget(d, null);
    ddFinish(d, false);
    $(d.item).trigger("ah:drag-cancel");
  }

  function ddLive(el) {
    var $live = $(el).children(".ah-dnd-live");
    if (!$live.length) {
      $live = $('<span class="ah-sortable-live ah-dnd-live" aria-live="assertive" aria-atomic="true"></span>')
        .appendTo(el);
    }
    return $live;
  }

  // Keyboard: Space/Enter picks up, arrows walk the zones, Space/Enter
  // drops, Escape (or leaving the item) cancels.
  function ddKeydown(el, item, e) {
    var d = drag && drag.kind === "dragdrop-key" && drag.item === item ? drag : null;
    var pick = e.key === " " || e.key === "Enter";
    if (!d) {
      if (pick && !drag && ddUsable(el, item)) {
        e.preventDefault();
        d = { kind: "dragdrop-key", el: el, item: item };
        ddBegin(d);
        if (!d.zones.length) {
          ddFinish(d, false);
          announce(ddLive(el), "No drop zone takes " + label(item) + ".");
          return;
        }
        drag = d;
        d.cancel = function () { drag = null; ddCancel(d); };
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
      drag = null;
      if (d.target) { ddDrop(d); } else { ddCancel(d); }
    } else if (e.key === "Escape") {
      e.preventDefault();
      d.cancel();
      announce(ddLive(el), "Cancelled.");
    }
  }

  AH.define("dragdrop", {
    init: function (el, $el) {
      $el.on("pointerdown" + NS, "[data-ah-drag]", function (e) {
        var item = this;
        if (drag || !ddScope(el, item) || !ddUsable(el, item) ||
            (e.pointerType === "mouse" && e.button !== 0) || editable(e.target)) {
          return;
        }
        if ($(e.target).closest("[data-ah-drag]")[0] !== item) { return; }
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
            $("body").addClass("ah-disableselect");
            ddBegin(d);
          }
          d.px = px(me); d.py = py(me); d.cx = me.clientX; d.cy = me.clientY;
          if (d.raf) { return; }
          d.raf = requestAnimationFrame(function () {
            d.raf = 0;
            if (drag !== d) { return; }
            $(d.copy).css({ left: (d.px - d.offX) + "px", top: (d.py - d.offY) + "px" });
            autoScroll(d.box, d.cx, d.cy);
            ddTarget(d, ddHit(d, d.px, d.py));
            $(item).trigger("ah:dragging", [{ pageX: d.px, pageY: d.py }]);
          });
        }, function (ue) {
          if (!d.started) { return; }
          swallowClick();
          // the last frame may not have run: test where the pointer let go
          $(d.copy).css({ left: (px(ue) - d.offX) + "px", top: (py(ue) - d.offY) + "px" });
          ddTarget(d, ddHit(d, px(ue), py(ue)));
          if (d.target) { ddDrop(d); } else { ddFinish(d, false); }
        }, function () {
          if (d.started) { ddCancel(d); }
        });
      });
      $el.on("keydown" + NS, "[data-ah-drag]", function (e) {
        if (e.target === this && ddScope(el, this)) { ddKeydown(el, this, e); }
      });
      $el.on("focusout" + NS, "[data-ah-drag]", function () {
        var item = this;
        setTimeout(function () {
          if (drag && drag.kind === "dragdrop-key" && drag.item === item &&
              document.activeElement !== item) {
            drag.cancel();
          }
        }, 0);
      });
    },
    destroy: function (el, $el) {
      cancelFor(el);
      $el.children(".ah-dnd-live").remove();
    },
    methods: {
      enable: function (el, $el) {
        $el.removeClass("ah-dragdrop-disabled").removeAttr("aria-disabled");
      },
      disable: function (el, $el) {
        cancelFor(el);
        $el.addClass("ah-dragdrop-disabled").attr("aria-disabled", "true");
      },
      cancel: function (el) { cancelFor(el); }
    }
  });
})(window.jQuery, window.AH);
