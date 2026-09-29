/* The sortable behaviour (designs/04-components.md), ported from sigil's
 * layout/sortable (+ sortable/geometry): reorder by dragging (or from the
 * keyboard); the value is the order of the item keys (data-value),
 * change after a drop that changed it; lists with the same data-ah-group
 * exchange items. The drag machinery is shared with dragdrop.js
 * (_lib_dnd.js).
 */
(function ($, AH) {
  "use strict";

  var NS = AH.NS;
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
      track = L.track,
      cancelFor = L.cancelFor;

  // ==================================================================
  // sortable
  // ==================================================================

  function soItems(list) {
    return $(list).children(".ah-sortable-item").get();
  }

  function soKey(item) { return item.getAttribute("data-value") || ""; }

  function soOrder(list) {
    return AH.lib.values.join(soItems(list).map(soKey));
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
      if (L.drag !== d) { return; }
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
      soSetOrder(list, AH.lib.values.split(st.order));
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
        if (item.parentNode !== el || L.drag || soDisabled(el) ||
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
        var keys = AH.lib.values.split(Array.isArray(order) ? order : String(order || ""))
          .filter(Boolean);
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
})(window.jQuery, window.AH);
